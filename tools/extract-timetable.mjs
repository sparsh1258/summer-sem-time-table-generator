// Converts the university's grid-style master timetable workbook into the
// flat JSON dataset embedded in the app (src/timetable.json).
//
// Usage: node tools/extract-timetable.mjs "<path to .xlsx>"
//
// Sheet layout (one block per room/lab):
//   <blank rows>
//   |   | TIMETABLE SLOT (ROOM NO.B301) | ... CAPACITY -143 |     <- or just a lab name like "EL LAB"
//   |DAY| 8:00AM | 8:50AM | ... | 6:50PM |                        <- 14 fifty-minute slots
//   |MONDAY | UEC301L | UEC301L | ... |                           <- codes in the day row itself
//   |       | G1      | G1      | ... |                           <- up to 4 qualifier rows (group/section/dept)
//   ...
import { readFile } from 'node:fs/promises';
import { writeFile } from 'node:fs/promises';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const XLSX = require('xlsx');

const file = process.argv[2];
if (!file) {
  console.error('usage: node tools/extract-timetable.mjs <xlsx>');
  process.exit(1);
}

const wb = XLSX.read(await readFile(file));
const ws = wb.Sheets[wb.SheetNames[0]];
const rows = XLSX.utils.sheet_to_json(ws, { header: 1, defval: '' });

const DAYS = ['MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 'SATURDAY', 'SUNDAY'];
const cell = (r, c) => String((rows[r] || [])[c] ?? '').replace(/\s+/g, ' ').trim();

// "8:50AM" / "12:10PM" -> minutes since midnight; Excel serial fractions too.
function toMinutes(v) {
  if (typeof v === 'number') return Math.round(v * 24 * 60);
  const m = String(v).trim().match(/^(\d{1,2}):(\d{2})\s*(AM|PM)$/i);
  if (!m) return null;
  let h = Number(m[1]) % 12;
  if (/pm/i.test(m[3])) h += 12;
  return h * 60 + Number(m[2]);
}

// "UES101/UTA015P" -> { code: "UES101/UTA015", type: "P" }
const CODE_RE = /^([A-Z]{2,4}\d{3}(?:\/[A-Z]{2,4}\d{3})*)\s*([LTP])$/;

// The sheet sometimes abbreviates a cross-listing: "UMA004/23L" means
// UMA004/UMA023. Expand the digits-only second half before matching.
const expandShorthand = (s) =>
  s.replace(/^([A-Z]{2,4})(\d{3})\/(\d{1,3})(\s*[LTP])$/, (_, p, n1, n2, t) =>
    `${p}${n1}/${p}${n2.padStart(3, '0')}${t}`);

// Locate blocks: any row whose following row starts with DAY in col 0,
// or a "TIMETABLE SLOT" header (B107 has no DAY row of its own).
const blocks = [];
for (let i = 0; i < rows.length; i++) {
  const headerText = (rows[i] || []).map(String).join(' ');
  const isSlotHeader = /TIMETABLE SLOT/i.test(headerText);
  const nextIsDayRow = cell(i + 1, 0).toUpperCase() === 'DAY';
  if (!isSlotHeader && !nextIsDayRow) continue;
  if (cell(i, 0).toUpperCase() === 'DAY') continue; // the DAY row itself
  const roomMatch = headerText.match(/ROOM NO\.?\s*([A-Z0-9 -]+?)\)?(\s|$)/i);
  const room = roomMatch ? roomMatch[1].trim() : (cell(i, 1) || cell(i, 0));
  if (!room || DAYS.includes(room.toUpperCase())) continue;
  blocks.push({ row: i, room });
}

// All DAY rows carry the same 14 slot times; read them once for fallback.
let defaultTimes = null;
const timesOfDayRow = (r) => {
  const t = [];
  for (let c = 1; c < (rows[r] || []).length; c++) {
    const mins = toMinutes((rows[r] || [])[c]);
    if (mins !== null) t[c] = mins;
  }
  return t;
};
for (let i = 0; i < rows.length && !defaultTimes; i++) {
  if (cell(i, 0).toUpperCase() === 'DAY') {
    const t = timesOfDayRow(i);
    if (t.filter(Boolean).length >= 10) defaultTimes = t;
  }
}

const entries = [];
const anomalies = [];

blocks.forEach((block, bi) => {
  const blockEnd = bi + 1 < blocks.length ? blocks[bi + 1].row : rows.length;
  let times = defaultTimes;
  if (cell(block.row + 1, 0).toUpperCase() === 'DAY') {
    const t = timesOfDayRow(block.row + 1);
    if (t.filter(Boolean).length >= 10) times = t;
  }

  // day rows inside this block
  const dayRows = [];
  for (let r = block.row + 1; r < blockEnd; r++) {
    const d = cell(r, 0).toUpperCase();
    if (DAYS.includes(d)) dayRows.push({ row: r, day: d });
  }

  dayRows.forEach((dr, di) => {
    const dayEnd = di + 1 < dayRows.length ? dayRows[di + 1].row : blockEnd;
    const maxCol = Math.max(...[dr.row, ...Array.from({ length: dayEnd - dr.row - 1 }, (_, k) => dr.row + 1 + k)]
      .map((r) => (rows[r] || []).length), 0);

    // raw per-column slot: code from the day row, qualifiers from sub-rows
    const slots = [];
    for (let c = 1; c < maxCol; c++) {
      const raw = cell(dr.row, c);
      if (!raw) continue;
      const m = expandShorthand(raw.toUpperCase()).match(CODE_RE);
      if (!m) {
        anomalies.push(`unparsed cell "${raw}" @ row ${dr.row + 1}, ${block.room} ${dr.day}`);
        continue;
      }
      const quals = [];
      for (let r = dr.row + 1; r < dayEnd; r++) {
        const q = cell(r, c);
        if (!q) continue;
        if (CODE_RE.test(q.toUpperCase()))
          anomalies.push(`code-like cell "${q}" in qualifier row ${r + 1}, ${block.room} ${dr.day}`);
        quals.push(q.toUpperCase().replace(/^G-(\d+)$/, 'G$1'));
      }
      if (!times || times[c] === undefined) {
        anomalies.push(`no slot time for column ${c} @ row ${dr.row + 1}, ${block.room}`);
        continue;
      }
      slots.push({ col: c, code: m[1], type: m[2], group: quals.join(' ') || 'ALL', start: times[c] });
    }

    // merge consecutive columns of the same class into one interval
    slots.sort((a, b) => a.col - b.col);
    let cur = null;
    const flush = () => { if (cur) entries.push(cur); cur = null; };
    for (const s of slots) {
      const end = s.start + 50;
      if (cur && cur.code === s.code && cur.type === s.type && cur.group === s.group && cur.endCol + 1 === s.col) {
        cur.end = end;
        cur.endCol = s.col;
      } else {
        flush();
        cur = { code: s.code, type: s.type, group: s.group, day: dr.day, start: s.start, end, room: block.room, endCol: s.col };
      }
    }
    flush();
  });
});

for (const e of entries) delete e.endCol;

// exact duplicates can appear when a sheet repeats a block — drop them
const seen = new Set();
const unique = entries.filter((e) => {
  const k = JSON.stringify(e);
  if (seen.has(k)) return false;
  seen.add(k);
  return true;
});

const codes = [...new Set(unique.map((e) => e.code))].sort();
const rooms = [...new Set(unique.map((e) => e.room))].sort();

await writeFile(new URL('../src/timetable.json', import.meta.url), JSON.stringify({
  semester: 'Summer Semester 2026',
  entries: unique,
}, null, 1));

console.log(`blocks: ${blocks.length}, entries: ${unique.length} (${entries.length - unique.length} dups dropped)`);
console.log(`courses: ${codes.length}, rooms/labs: ${rooms.length}`);
console.log('codes:', codes.join(' '));
console.log('rooms:', rooms.join(' | '));
if (anomalies.length) {
  console.log(`--- anomalies: ${anomalies.length}`);
  [...new Set(anomalies)].slice(0, 50).forEach((a) => console.log(' ', a));
}
