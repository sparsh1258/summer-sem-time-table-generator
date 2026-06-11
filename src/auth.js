// SHA-256 helper for the admin password gate. Only the hash of the password
// is stored in the source — never the plaintext. Note: in a fully
// client-side app this is a soft gate (the JS is readable by anyone);
// for hard protection put the deployment behind hosting-level auth.

export async function sha256Hex(text) {
  const data = new TextEncoder().encode(text);
  const buf = await crypto.subtle.digest('SHA-256', data);
  return [...new Uint8Array(buf)].map((b) => b.toString(16).padStart(2, '0')).join('');
}
