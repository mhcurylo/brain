import { should } from 'chai';
import { initState } from '../../../state/state.init';
import { Place, State } from '../../../state/state.interface';
import { initStateWithPages, pageEventAction, pageShownAction, placeOfErr } from '../../../testing/fixtures';
import { PageShownAction } from './actions.interface';
import { pageShownEventAction } from './pageShownEvent.action';

should();

describe('pageShownEventAction', () => {
    it('should not modify the old state, but create a new one', () => {
        const newState: State = pageShownEventAction(initState, pageShownAction);

        newState.should.not.equal(initState);
        newState.should.not.eql(initState);
    });

    it('should create a new page entry, if none exists', () => {
        const newState: State = pageShownEventAction(initState, pageShownAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [],
            shown: pageShownAction.payload.shown,
        });
    });

    it('should update the shown prop of existing page entry', () => {
        const newState: State = pageShownEventAction(initStateWithPages, pageShownAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
            shown: pageShownAction.payload.shown,
        });
    });
});
