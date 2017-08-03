import { Place, State } from '../../../state/state.interface';
import { CanonicalAction } from './actions.interface';

export const canonicalAction = (state: State, {payload: {originalUrl, canonicalUrl}}: CanonicalAction) => {
    const canonical = state.canonical;

    // To do: use immutable map.
    canonical.set(originalUrl, canonicalUrl);

    return {...state, canonical};
}
