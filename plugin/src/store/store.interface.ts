export interface ActionEvent {
    readonly kind: string;
    readonly payload: object;
}

export type Subscription<T> = (state: T, action: ActionEvent) => void;

export type Reducer<M> = (state: M, action: ActionEvent) => M;

export interface MetaState<T> {
    readonly reducers: Array<Reducer<MetaState<T>>>;
    readonly state: T;
    readonly subscriptions: Array<Subscription<T>>;
}

export type ActionStore<T> = (event: ActionEvent) => ActionStore<T>;

export type Store<T> = (state: MetaState<T>) => (event: ActionEvent) => ActionStore<T>;
