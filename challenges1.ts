function identity<X>(x: X) {
  return x;
}

function compose<A, B, C>(f: (a: B) => C, g: (a: A) => B): (a: A) => C {
  return (a: A) => f(g(a));
}

function numberToString(a: number): string { return a + ""; }

function stringToNumber(a: string): number { return parseFloat(a); }

// to check that the type of x1 equals the type of x2
function isAssociative<X>(x1: X, x2: X) {}; 

function proofCompose() { 
  const idL: typeof numberToString = compose(identity, numberToString);
  const idR: typeof numberToString = compose(numberToString, identity);
  isAssociative(
    compose(numberToString, compose(stringToNumber ,numberToString)), 
    compose(compose(numberToString, stringToNumber), numberToString)
  );
}

