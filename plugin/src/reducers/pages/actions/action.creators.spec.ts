import { should } from 'chai';
import { pageEventAction, pageShownAction } from '../../../testing/fixtures';
import { addPageEvent, pageShownEvent } from './action.creators';
import { PageEventAction, PageShownAction } from './actions.interface';

should();

describe('Action creators', () => {
    describe('PageEventAction action creator', () => {
        it('should take data and return a PageEventAction', () => {
            const action: PageEventAction = addPageEvent(pageEventAction.payload);

            action.should.be.eql(pageEventAction);
        });
    })
    describe('PageShownAction action creator', () => {
        it('should take data and return a PageShownAction', () => {
            const action: PageShownAction = pageShownEvent(pageShownAction.payload.place,
                                                           pageShownAction.payload.shown);

            action.should.be.eql(pageShownAction);
        });
    })
})
