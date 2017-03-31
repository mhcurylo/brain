export interface Place {
    readonly title: string;
    readonly url: string;
}

export interface PageEvent {
    readonly at: Place;
    readonly from: Place;
    readonly req: Place;
    readonly who: string;
}

export interface Page {
    readonly at: Place;
    readonly events: PageEvent[];
}

export interface Pages {
    readonly [key: string]: Page;
}

export interface State {
    readonly pages: Pages;
    readonly who: string;
}
