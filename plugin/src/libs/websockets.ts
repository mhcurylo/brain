import { fmap, isNothing, Maybe } from './maybe';

export const initSockets = (url: string, callback: ((res: MessageEvent) => void)) => {
    let ws: WebSocket = serverConnect();
    let timeout: Maybe<number> = null;
    let helloBrain: () => void = shakeWith(null);

    function serverConnect(): WebSocket {
        const nws = new WebSocket(url);

        nws.onclose = reconnect;
        nws.onerror = reconnect;
        nws.onmessage = callback;
        nws.onopen = () => helloBrain();

        return nws;
    }

    function reconnect() {
        if (!timeout) {
            timeout = setTimeout(() => {
                ws = serverConnect();
                timeout = null;
            }, 1000);
        }
    }

    function shakeWith(msg: Maybe<string>): (() => void) {
        return () => {
            ws.send('Hello Brain!');
            if (!isNothing(msg)) { ws.send(msg) };
        };
    }

    return (req: string): void => {
        if (ws.readyState === 1) {
            ws.send(req);
        } else {
            helloBrain = shakeWith(req);
        }
    };
}
