import { Maybe } from '../libs/maybe';
import { getCanonicalUrl, Page, PageEvent, Place, State } from '../state/state.interface';
import { ActionEvent } from '../store/store.interface';
import { renderBadgeText } from '../views/badge.view';

export const updateBadge = (state: State, action: ActionEvent): void => {
    chrome.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
        const tabUrl = tabs[0] ? tabs[0].url : null;
        const url: Maybe<string> = getCanonicalUrl(state)(tabUrl);
        if (url && state.pages[url]) {
            const page: Page = state.pages[url];
            const text: string = renderBadgeText(page);

            chrome.browserAction.setBadgeText({text});
        }
    });
};
