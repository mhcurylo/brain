export interface ActionEvent {
    kind: string;
    payload: object;
}

export interface Subscription<T> {
    (state: T, actions: ActionEvent[]): void;
}

export interface Reducer<M> {
    (state: M, action: ActionEvent): M;
}

export interface MetaState<T> {
    actions: ActionEvent[],
    reducers: Reducer<MetaState<T>>[]
    state: T;
    subscribers: Subscription<T>[]
}

export interface ActionStore<T> {
    (event: ActionEvent): ActionStore<T>;
}

export interface Store<T> {
    (state: MetaState<T>): ((event: ActionEvent) => ActionStore<T>);
}


