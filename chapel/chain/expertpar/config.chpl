module Config {
  const randSpace = [1..20000, 1..20000];
  const histSpace = [0..99];
  const pointSpace = [1..20000];
  const distSpace = [1..10000, 1..10000];
  const vectorSpace = [1..10000];

  var matrix: [randSpace] int;
  var mask: [randSpace] bool;
  var points: [pointSpace] (int, int);
  var dists: [distSpace] real;
  var vector, result: [vectorSpace] real;
}
