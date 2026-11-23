// Keeps a reference to d3 version 3 (loaded just before this file) as window.d3v3, so that rvis widgets built on
// the d3 v3 API (coffeewheel, TFD3) keep working when another widget later loads a newer d3 as window.d3.
window.d3v3 = window.d3;
