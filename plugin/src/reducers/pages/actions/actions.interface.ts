import { PageEvent, Place } from '../../../state/state.interface';
import { ActionEvent } from '../../../store/store.interface';

export type PR_ACTION_TYPE = 'PAGE_EVENT_ACTION' | 'PAGE_SHOWN_ACTION' | 'EMPTY_ACTION' | 'CANONICAL_URL_ACTION';

export interface PageEventAction extends ActionEvent {
    kind: 'PAGE_EVENT_ACTION';
    payload: PageEvent;
}

export interface PageShownAction extends ActionEvent {
    kind: 'PAGE_SHOWN_ACTION';
    payload: {
        place: Place;
        shown: number;
    };
}

export interface CanonicalAction extends ActionEvent {
    kind: 'CANONICAL_URL_ACTION';
    payload: {
        original: string;
        canonical: string;
    };
}

export interface EmptyAction extends ActionEvent {
    kind: 'EMPTY_ACTION';
    payload: {};
}

export const isPageEventAction = (event: ActionEvent): event is PageEventAction => event.kind === 'PAGE_EVENT_ACTION';
export const isPageShownAction = (event: ActionEvent): event is PageShownAction => event.kind === 'PAGE_SHOWN_ACTION';
export const isCanonicalAction = (event: ActionEvent): event is CanonicalAction =>
    event.kind === 'CANONICAL_URL_ACTION';
export const isEmptyAction = (event: ActionEvent): event is PageShownAction => event.kind === 'EMPTY_ACTION';
