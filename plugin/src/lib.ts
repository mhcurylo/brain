
export type Nothing = undefined | null;
export type Just <a> = a;
export type Maybe <a> = Just <a> | Nothing;

export function fmap<A, B>(f: ((t: A) => B)): ((t:Maybe<A>) => Maybe<B>) {
    return x => isNothing(x) ? null : f(x);
};

function isNothing(x: any): x is Nothing {
    return (x === null || x === undefined);
}
