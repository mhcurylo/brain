import { PageEvent, Place } from '../../../state/state.interface';
import { PageEventAction, PageShownAction } from './actions.interface';

export const addPageEvent = (pageEvent: PageEvent): PageEventAction =>
    ({kind: 'PAGE_EVENT_ACTION', payload: pageEvent});

export const pageShownEvent = (place: Place, shown: number): PageShownAction => ({
    kind: 'PAGE_SHOWN_ACTION',
    payload: {place, shown},
});
