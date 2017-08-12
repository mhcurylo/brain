import { Place, State } from '../../../state/state.interface';
import { CanonicalAction } from './actions.interface';

export const canonicalAction = (state: State, {payload}: CanonicalAction) => {

    // To do: use immutable map.
    const canonical = state.canonical.set(payload.original, payload.canonical);

    return {...state, canonical};
}
