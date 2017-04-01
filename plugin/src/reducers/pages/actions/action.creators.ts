import { PageEvent } from '../../../state/state.interface';
import { PageEventAction } from './actions.interface';

export const addPageEvent = (pageEvent: PageEvent): PageEventAction =>
    ({kind: 'PAGE_EVENT_ACTION', payload: pageEvent});
