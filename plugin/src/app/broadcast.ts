import { State } from '../state/state.interface';
import { ActionEvent } from '../store/store.interface';

export const broadcast = (state: State, action: ActionEvent): void => {
    const views = chrome.extension.getViews();
    views.forEach((view: any) => view.updateState ? view.updateState(state) : '');
}
