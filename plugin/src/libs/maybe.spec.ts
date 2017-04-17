import { should } from 'chai';
import { fmap, Maybe } from './maybe';
const s = should();

describe('Fmap', () => {
    const a: Maybe<number> = 1;
    const b: Maybe<number> = 2;
    const c: Maybe<number> = null;
    const d: Maybe<number> = undefined;
    const add = (x: number, y: number) => x + y;

    const fmapedAdd = fmap(add);

    it('should return a function which accepts maybe values', () => {
        // tested by type;
        const e: Maybe<number> = fmapedAdd(a, b);
        (typeof fmapedAdd).should.eql('function');
    });

    it('should return null if any of arguments is null or undefined', () => {
        const e: Maybe<number> = fmapedAdd(a, c);
        const f: Maybe<number> = fmapedAdd(d, a);

        s.not.exist(e);
        s.not.exist(f);
    });

    it('should return the value of the fmaped function if arguments are non null', () => {
        s.equal(add(a, b), fmapedAdd(a, b));
    });

});
