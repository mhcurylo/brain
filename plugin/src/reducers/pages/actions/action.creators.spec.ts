import { should } from 'chai';
import { pageEventAction } from '../../../testing/fixtures';
import { addPageEvent } from './action.creators';
import { PageEventAction } from './actions.interface';

should();

describe('Action creators', () => {
    describe('PageEventAction action creator', () => {
        it('should take data and return a PageEventAction', () => {
            const action: PageEventAction = addPageEvent(pageEventAction.payload);

            action.should.be.eql(pageEventAction);
        });
    })
})
