import { Maybe } from '../libs/maybe';
import { Page, PageEvent, Place, State } from '../state/state.interface';
import { ActionEvent } from '../store/store.interface';
import { renderBadgeText } from '../views/badge.view';

export const updateBadge = (state: State, action: ActionEvent): void => {
    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const url: Maybe<string> = tabs[0].url;

        if (url && state.pages[url]) {
            const page: Page = state.pages[url];
            const text: string = renderBadgeText(page);

            chrome.browserAction.setBadgeText({text});
        }
    });
};
