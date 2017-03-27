interface ActionEvent {
    type: string;
    payload: object;
}

interface Subscription<T> {
    (state: T, actions: ActionEvent[]): void;
}

interface Reducer<M> {
    (state: M, action: ActionEvent): M;
}

interface MetaState<T> {
    actions: ActionEvent[],
    reducers: Reducer<MetaState<T>>[]
    state: T;
    subscribers: Subscription<T>[]
}

interface Store<T> {
    (state: MetaState<T>): ((event: ActionEvent) => Store<T>);
}


