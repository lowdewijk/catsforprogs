function memoize<A extends string | number, B>(fn: (a: A) => B): (a: A) => B {
  let mem: { [a: string | number]: B} = {};
  return a => {
    const cachedA = mem[a];
    if (cachedA) {
      console.log("cache hit");
      return cachedA;
    } else {
      const b = fn(a);
      mem[a] = b;
      return b;
    }
  }
}

const memoizedParsedInt = memoize(parseInt);
memoizedParsedInt("42");
memoizedParsedInt("42");