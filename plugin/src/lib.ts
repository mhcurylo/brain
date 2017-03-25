interface Functor<T> {
    map<U>(f: (t: T) => U): Functor<U>;
}

export class Maybe<T> implements Functor<T> {
    constructor (public v: T | null | undefined) {
    };

    static of<W>(v: W | null): Maybe<W> {
        return v ? Just.of(v) : new Nothing<W>(null);
    }

    map<U>(f: (t: T) => U): Maybe<U> {
        return this.v ? Maybe.of(f(this.v)) : new Nothing<U>(null);
    }
}

export class Nothing<T> extends Maybe<T> {
    public v: null | undefined;

    map<U>(f: (t: T) => U): Nothing<U> {
        return new Nothing<U>(null);
    }
}

export class Just<T> extends Maybe<T> {
    public v: T;

    static of<U>(v: U): Just<U> {
        return new Just<U>(v);
    }

    map<U>(f: (t: T) => U): Maybe<U> {
        return Maybe.of(f(this.v));
    }
}

