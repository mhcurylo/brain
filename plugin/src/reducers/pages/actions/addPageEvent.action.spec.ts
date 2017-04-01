import { should } from 'chai';
import { initState } from '../../../state/state.init';
import { Place, State } from '../../../state/state.interface';
import { initStateWithPages, pageEventAction, placeOfErr } from '../../../testing/fixtures';
import { PageEventAction } from './actions.interface';
import { addPageEvent } from './addPageEvent.action';

should();

describe('addPageEvent', () => {
    it('should not modify the old state, but create a new one', () => {
        const newState: State = addPageEvent(initState, pageEventAction);

        initState.should.equal(initState);
        initState.should.eql(initState);
        newState.should.not.equal(initState);
        newState.should.not.eql(initState);
    });

    it('should add a page entry with the new event if page entry is not present', () => {
        const newState: State = addPageEvent(initState, pageEventAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
        });
    });

    it('should add a PageEvent to the appropriate page entry if page entry is present', () => {
        const newState: State = addPageEvent(initStateWithPages, pageEventAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload, pageEventAction.payload],
        });
    });
});
