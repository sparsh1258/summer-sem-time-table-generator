// Renders the on-screen timetable card to a downloadable PDF file.
// jsPDF + html2canvas are imported lazily so normal visitors never pay for
// them — they only load the first time someone clicks "Download PDF".

export async function downloadTimetablePdf(elementId, filename) {
  const el = document.getElementById(elementId);
  if (!el) return;

  const [{ default: html2canvas }, { jsPDF }] = await Promise.all([
    import('html2canvas'),
    import('jspdf'),
  ]);

  const canvas = await html2canvas(el, { scale: 2, backgroundColor: '#ffffff' });
  const w = canvas.width / 2;
  const h = canvas.height / 2;

  const pdf = new jsPDF({
    orientation: w >= h ? 'landscape' : 'portrait',
    unit: 'px',
    format: [w, h],
  });
  pdf.addImage(canvas.toDataURL('image/png'), 'PNG', 0, 0, w, h);
  pdf.save(filename);
}
