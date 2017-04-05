import { Maybe } from '../libs/maybe';
import { Page, PageEvent, Place, State } from '../state/state.interface';
import { ActionEvent } from '../store/store.interface';

export const renderBadgeText = (page: Page): string => {
    const shown: number = page.shown;
    const newEvents: number =  page.events.filter((e: PageEvent): boolean => e.when > shown).length;

    if (newEvents > 99) {
        return '+99';
    } else if (newEvents > 0) {
        return  newEvents.toString();
    } else {
        return '';
    }
}

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
