import { should } from 'chai';
import { initState } from '../../../state/state.init';
import { Place, State } from '../../../state/state.interface';
import { initStateWithPages, pageShownAction, pageEventAction, placeOfErr } from '../../../testing/fixtures';
import { PageShownAction } from './actions.interface';
import { pageShownEventAction } from './pageShownEvent.action';

should();

describe('pageShownEventAction', () => {
    it('should not modify the old state, but create a new one', () => {
        const newState: State = pageShownEventAction(initState, pageShownAction);

        initState.should.equal(initState);
        initState.should.eql(initState);
        newState.should.not.equal(initState);
        newState.should.not.eql(initState);
    });

    it('should add a page entry with the new event if page entry is not present', () => {
        const newState: State = pageShownEventAction(initState, pageShownAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [],
            shown: pageShownAction.payload.shown,
        });
    });

    it('should add a PageEvent to the appropriate page entry if page entry is present', () => {
        const newState: State = pageShownEventAction(initStateWithPages, pageShownAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
            shown: pageShownAction.payload.shown,
        });
    });
});
