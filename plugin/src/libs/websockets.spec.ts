import { should } from 'chai';
import { Server } from 'mock-socket';
import * as sinon from 'sinon';
import { initSockets } from './websockets';

should();

describe('Webscokets', () => {
    let clock: sinon.SinonFakeTimers;
    let response: string = '';
    let mockServer: any;
    let connected: boolean;
    let receivedMsgs: string[] = [];
    const callback = (res: MessageEvent) => response = res.data;

    before(() => {
        clock = sinon.useFakeTimers();
    });

    after(() => {
        clock.restore();
    })

    beforeEach(() => {
        connected = false;
        mockServer = new Server('ws://localhost:3000');
        mockServer.on('connection', (server: any) => {
            mockServer.send('Boo');
            connected = true;
        });
        mockServer.on('message', (msg: any) => {
            receivedMsgs.push(msg);
        })
    });

    afterEach(() => {
        receivedMsgs = [];
        mockServer.stop();
    })

    it('should connect to websocket server', () => {
        initSockets('ws://localhost:3000', callback);

        clock.tick(100);
        connected.should.eql(true);
    });

    it('should call the callback with the received MessageEvent', () => {
        initSockets('ws://localhost:3000', callback);

        clock.tick(100);
        response.should.eql('Boo');
    });

    it('should shake with "Hello Brain!" on first connection', () => {
        initSockets('ws://localhost:3000', callback);

        clock.tick(100);
        receivedMsgs.should.contain('Hello Brain!');
    });

    it('should shake with "Hello Brain!" and a message, if the message is sent before first connection', () => {
        initSockets('ws://localhost:3000', callback)('Foo');

        clock.tick(100);
        receivedMsgs.should.contain('Hello Brain!');
        receivedMsgs.should.contain('Foo');
    });

    it('should shake with "Hello Brain!" and the latest message, if messages were sent before first connection', () => {
        const send = initSockets('ws://localhost:3000', callback);
        send('Foo');
        send('Woo');

        clock.tick(100);
        receivedMsgs.should.contain('Hello Brain!');
        receivedMsgs.should.contain('Woo');
        receivedMsgs.should.not.contain('Foo');
    });

    it('should be able to send any number of messages after connection', () => {
        const send = initSockets('ws://localhost:3000', callback);
        send('Foo');
        clock.tick(100);
        send('Woo');
        send('Hoo');
        send('Ooo');
        clock.tick(100);
        receivedMsgs.should.contain('Hello Brain!');
        receivedMsgs.should.contain('Foo');
        receivedMsgs.should.contain('Woo');
        receivedMsgs.should.contain('Hoo');
        receivedMsgs.should.contain('Ooo');
    });

    it('should reconnect on close sending the last message', () => {
        const send = initSockets('ws://localhost:3000', callback);
        send('Foo');
        clock.tick(100);

        mockServer.close();
        mockServer = new Server('ws://localhost:3000');
        mockServer.on('message', (msg: any) => {
            receivedMsgs.push(msg);
        });

        send('Woo');
        send('Hoo');

        clock.tick(1100);

        receivedMsgs.should.contain('Foo');
        receivedMsgs.should.not.contain('Woo');
        receivedMsgs.should.contain('Hoo');
   });

    it('should reconnect on error', () => {
        mockServer.stop();
        clock.tick(10);
        const send = initSockets('ws://localhost:3000', callback);
        send('Foo');
        send('Woo');
        clock.tick(2000);

        mockServer = new Server('ws://localhost:3000');
        mockServer.on('message', (msg: any) => {
            receivedMsgs.push(msg);
        })

        clock.tick(1100);

        receivedMsgs.should.not.contain('Foo');
        receivedMsgs.should.contain('Woo');
   });
});
