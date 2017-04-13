import { should } from 'chai';
import { Page, Place, State } from '../state/state.interface';
import { fullBlownPage, pandaArrived, pandaDeparted, pandaTraveled } from '../testing/fixtures';
import { renderBadgeText }from './badge.view';

should();

describe('Badge view', () => {
    describe('renderBadgeText ', () => {
        const pandas99 = Array.from({length: 99}, () => pandaTraveled);

        it('should return empty string if no new events occurred', () => {
            const events = [pandaArrived, pandaDeparted];
            const text = renderBadgeText({...fullBlownPage, events});

            text.should.eql('');
        });

        it('should return the number of new events if number new events < 99', () => {
            const events = pandas99;
            const text = renderBadgeText(fullBlownPage);
            const text2 = renderBadgeText({...fullBlownPage, events});

            text.should.eql('1');
            text2.should.eql('99');
        });

        it('should return +99 if there are more new events than 99', () => {
            const events = [...pandas99, pandaTraveled]
            const text = renderBadgeText({...fullBlownPage, events});

            text.should.eql('+99');
        });
    })
});

