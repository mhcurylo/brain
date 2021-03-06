import { should } from 'chai';
import { initState } from '../../state/state.init';
import { State } from '../../state/state.interface';
import { ActionEvent, Reducer } from '../../store/store.interface';
import { emptyAction, initStateWithPages, pageEventAction, pageShownAction, placeOfErr } from '../../testing/fixtures';
import { pagesReducer } from './pages.reducer';

should();

describe('pageReducer', () => {
    it('should add a PageEvent to the appropriate page entry', () => {
        const newState: State = pagesReducer(initState, pageEventAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
            shown: 0,
        });
    });

    it('should update the shown prop of existing page entry', () => {
        const newState: State = pagesReducer(initStateWithPages, pageShownAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
            shown: pageShownAction.payload.shown,
        });
    });

    it('should return unmodified state if the ActionEvent does not match any task', () => {
        const newState = pagesReducer(initState, emptyAction);

        newState.should.eql(initState);
        newState.should.equal(initState);
    });
});
