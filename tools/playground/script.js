const go = new Go();
let loaded = WebAssembly.instantiateStreaming(fetch("mochi.wasm"), go.importObject).then((result) => {
  go.run(result.instance);
});

document.getElementById("run").addEventListener("click", async () => {
  await loaded;
  const src = document.getElementById("source").value;
  const out = runMochi(src);
  document.getElementById("output").textContent = out;
});
