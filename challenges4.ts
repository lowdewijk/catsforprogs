type Some<A> = { val: A };
type None = {};
const None = {};
type Option<A> = Some<A> | None;

function Some<A>(a: A): Option<A> {
  return { val: a };
}

function isSome<A>(optA: Option<A>): optA is Some<A> {
  return optA.hasOwnProperty("val");
}

type KleisliMorphism<A, B> = (a: A) => Option<B>;
type KleisliIdentity<A> = KleisliMorphism<A, A>; // see the function `Some`;

function composeKleisli<A, B, C>(f: KleisliMorphism<B, C>, g: KleisliMorphism<A, B>): KleisliMorphism<A, C> {
  return a => {
    const b = g(a);
    if (isSome(b)) {
      return f(b.val);
    } else {
      return None as Option<C>;
    }
  }
}

function safe_reciprocal(x: number): Option<number> {
  if (x === 0) {
    return None;
  } else {
    return Some(1 / x);
  }
}

function safe_root(x: number): Option<number> {
  if (x >= 0) {
    return Some(Math.sqrt(x));
  } else {
    return None;
  }
 }

 const safe_root_reciprocal= composeKleisli(safe_root, safe_reciprocal);