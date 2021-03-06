import { State } from '../../state/state.interface';
import { ActionEvent, Reducer } from '../../store/store.interface';
import { isPageEventAction, isPageShownAction, isCanonicalAction } from './actions/actions.interface';
import { addPageEventAction } from './actions/addPageEvent.action';
import { canonicalAction } from './actions/canonicalEvent.action';
import { pageShownEventAction } from './actions/pageShownEvent.action';

export const pagesReducer: Reducer<State> = (state: State, event: ActionEvent): State => {
    if (isPageEventAction(event)) {
        return addPageEventAction(state, event);
    } else if (isPageShownAction(event)) {
        return pageShownEventAction(state, event);
    } else if (isCanonicalAction(event)) {
        return canonicalAction(state, event);
    } else {
        return state;
    }
};
