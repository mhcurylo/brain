import { should } from 'chai';
import { Page, Place, State } from '../state/state.interface';
import { fullBlownPage, pandaArrived, pandaDeparted, pandaTraveled } from '../testing/fixtures';
import { redrawPopup, renderPopup } from './popup.view';

should();

describe('Popup view', () => {
    describe('renderPopup ', () => {
        const rendered = renderPopup(fullBlownPage);

        it('should take a page state and return a string of HTML Markup starting with page title', () => {
            rendered.should.contain(fullBlownPage.at.title);
        });

        it('should render arrivals in "arrived from" format', () => {
            const regEx = new RegExp(`Panda arrived from.*${pandaArrived.from.title}`);

            rendered.should.match(regEx);
        });

        it('should render departures in "departed for" format', () => {
            const regEx = new RegExp(`Panda departed for.*${pandaDeparted.at.title}`);

            rendered.should.match(regEx);
        });

        it('should render other travels in "travelled from to" format', () => {
            const {from, at} = pandaTraveled;
            const regEx = new RegExp(`Panda travelled from.*${from.title}.*to.*${at.title}`);

            rendered.should.match(regEx);
        });
    });
});
