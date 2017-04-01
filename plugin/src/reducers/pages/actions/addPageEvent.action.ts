import { State } from '../../../state/state.interface';
import { PageEventAction } from './actions.interface';

export const addPageEvent = (state: State, {payload}: PageEventAction) => {
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
