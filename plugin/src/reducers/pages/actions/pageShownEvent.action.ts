import { Place, State } from '../../../state/state.interface';
import { PageShownAction } from './actions.interface';

export const pageShownEventAction = (state: State, {payload: {place, shown}}: PageShownAction) => {
    let pages = {...state.pages};
    const url = place.url;

    if (pages[url]) {
        const targetPage = {...pages[url], shown};
        pages = {...pages, [url]: targetPage};
    } else {
        pages = {...pages, [url]: {at: place, shown,  events: []}};
    }

    return {...state, pages};
}
