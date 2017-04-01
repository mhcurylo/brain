import { Page, PageEvent, Pages, Place, State } from '../state/state.interface';
import { ActionEvent, MetaState, Reducer } from '../store/store.interface';

export const PR_ACTION_TYPE = 'PAGE_EVENT';

interface PageEventAction extends ActionEvent {
    kind: 'PAGE_EVENT';
    payload: PageEvent;
}

const isPageEventAction = (event: ActionEvent): event is PageEventAction => event.kind === 'PAGE_EVENT';

const addPageEvent = (state: State, {payload}: PageEventAction) => {
    const targetUrl = payload.req.url;
    let pages = {...state.pages};

    if (pages[targetUrl]) {
        const events = [payload, ...pages[targetUrl].events];
        const targetPage = {...pages[targetUrl], events};
        pages = {...pages, [targetUrl]: targetPage};
    } else {
        pages = {...pages, [targetUrl]: {at: payload.req, events: [payload]}};
    }

    return {...state, pages};
}

export const pagesReducer: Reducer<State> = (state: State, event: ActionEvent): State => {
    if (isPageEventAction(event)) {
        return addPageEvent(state, event);
    } else {
        return state;
    }
};
