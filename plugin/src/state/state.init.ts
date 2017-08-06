import {State} from './state.interface';

export const initState: State = {
    canonical: new Map<string, string>(),
    pages: {},
    who: 'Waiting for name',
}
