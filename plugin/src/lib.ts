export interface Func<A, B> {
    (t: A): B
}

export type Nothing = undefined | null;
export type Just <a> = a;
export type Maybe <a> = Just <a> | Nothing;

export function fmap<A, B>(f: Func<A, B>): Func<Maybe<A>, Maybe<B>> {
    return x => isNothing(x) ? null : f(x);
};

function isNothing(x: any): x is Nothing {
    return (x === null || x === undefined);
}



