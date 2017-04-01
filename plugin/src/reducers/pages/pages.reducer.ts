import { State } from '../../state/state.interface';
import { ActionEvent, Reducer } from '../../store/store.interface';
import { isPageEventAction } from './actions/actions.interface';
import { addPageEvent } from './actions/addPageEvent.action';

export const pagesReducer: Reducer<State> = (state: State, event: ActionEvent): State => {
    if (isPageEventAction(event)) {
        return addPageEvent(state, event);
    } else {
        return state;
    }
};
