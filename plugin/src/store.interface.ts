export interface ActionEvent {
    readonly kind: string;
    readonly payload: object;
}

export interface Subscription<M> {
    (mstate: M, action: ActionEvent): void;
}

export interface Reducer<M> {
    (mstate: M, action: ActionEvent): M;
}

export interface MetaState<T> {
    readonly reducers: Array<Reducer<MetaState<T>>>;
    readonly state: T;
    readonly subscriptions: Array<Subscription<MetaState<T>>>;
}

export interface ActionStore<T> {
    (event: ActionEvent): ActionStore<T>;
}

export interface Store<T> {
    (state: MetaState<T>): ((event: ActionEvent) => ActionStore<T>);
}


