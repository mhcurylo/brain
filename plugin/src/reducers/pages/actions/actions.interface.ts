import { PageEvent } from '../../../state/state.interface';
import { ActionEvent } from '../../../store/store.interface';

export type PR_ACTION_TYPE = 'PAGE_EVENT_ACTION';

export interface PageEventAction extends ActionEvent {
    kind: 'PAGE_EVENT_ACTION';
    payload: PageEvent;
}

export const isPageEventAction = (event: ActionEvent): event is PageEventAction => event.kind === 'PAGE_EVENT_ACTION';
