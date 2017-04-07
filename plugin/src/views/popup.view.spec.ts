import { should } from 'chai';
import { Page, Place, State } from '../state/state.interface';
import { fullBlownPage } from '../testing/fixtures';
import { redrawPopup, renderPopup } from './popup.view';

should();

describe('Popup view', () => {
    describe('renderPopup', () => {
        it('should take a page state and return a string of HTML Markup starting with page title', () => {
            const rendered = renderPopup(fullBlownPage);

            rendered.should.contain(fullBlownPage.at.title);
        });
    });
});
