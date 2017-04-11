type Func1<A, Z> = (a: A) => Z
type Func2<A, B, Z> = (a: A, b: B) => Z
type Func3<A, B, C, Z> = (a: A, b: B, c: C) => Z

type Func<A, B, C, Z> = Func1<A, Z> | Func2<A, B, Z> | Func3<A, B, C, Z>

export type Nothing = undefined | null;
export type Just<a> = a;
export type Maybe<a> = Just<a> | Nothing;

export function fmap<A, Z>(f: Func1<A, Z>): Func1<Maybe<A>, Maybe<Z>>;
export function fmap<A, B, Z>(f: Func2<A, B, Z>): Func2<Maybe<A>, Maybe<B>, Maybe<Z>>;
export function fmap<A, B, C, Z>(f: Func3<A, B, C, Z>): Func3<Maybe<A>, Maybe<B>, Maybe<C>, Maybe<Z>>;
export function fmap(f: Function): Function {
    return (...args: any[]) => !args.some(isNothing) ? f.apply(null, args) : null;
}

export function isNothing(x: any): x is Nothing {
    return (x === null || x === undefined);
}
