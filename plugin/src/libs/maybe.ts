export interface Func1<A, B> {
    (a: A): B
}
export interface Func2<A, B, C> {
    (a: A, b: B): C
}

export type Nothing = undefined | null;
export type Just <a> = a;
export type Maybe <a> = Just <a> | Nothing;

export function fmap<A, B>(f: Func1<A, B>): Func1<Maybe<A>, Maybe<B>> {
    return (x) => isNothing(x) ? null : f(x);
};

function isNothing(x: any): x is Nothing {
    return (x === null || x === undefined);
}
