import { should } from 'chai';
import { initState } from '../../../state/state.init';
import { State, Place } from '../../../state/state.interface';
import { PageEventAction } from './actions.interface';
import { addPageEvent } from './addPageEvent.action';

should();

describe('addPageEvent', () => {
    const placeOfErr: Place = {
        title: 'A place of err',
        url: 'http://here.there.er',
    }

    const placeOfTher: Place = {
        title: 'A place of err',
        url: 'http://there.er',
    }

    const pageEventAction: PageEventAction = {
        kind: 'PAGE_EVENT_ACTION',
        payload: {
            at: placeOfErr,
            from: placeOfTher,
            req: placeOfErr,
            who: 'panda',
        }
    };

    it('should not modify the old state, but create a new one', () => {
        const newState: State = addPageEvent(initState, pageEventAction);

        initState.should.equal(initState);
        initState.should.eql(initState);
        newState.should.not.equal(initState);
        newState.should.not.eql(initState);
    })

    it('should add a page entry with the new event if page entry is not present', () => {
        const newState: State = addPageEvent(initState, pageEventAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload],
        });
    });

    it('should add a PageEvent to the appropriate page entry if page entry is present', () => {
        const initState: State = {
            pages: {
                'http://here.there.er': {
                    at: placeOfErr,
                    events: [pageEventAction.payload],
                },
                'http://there.er': {
                    at: placeOfTher,
                    events: [],
                },
            },
            who: 'meah',
        }

        const newState: State = addPageEvent(initState, pageEventAction);

        newState.pages['http://here.there.er'].should.eql({
            at: placeOfErr,
            events: [pageEventAction.payload, pageEventAction.payload],
        });
    });
});
