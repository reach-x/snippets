"""
tornado - Async web framework
Install: pip install tornado

Async web framework with WebSocket, long polling, and non-blocking I/O
"""

import tornado.ioloop
import tornado.web
import tornado.websocket
import tornado.httpserver
from tornado import gen
import json


# ===== Basic Request Handlers =====

class MainHandler(tornado.web.RequestHandler):
    """Basic GET handler"""

    def get(self):
        self.write("<h1>Hello from Tornado!</h1>")


class JSONHandler(tornado.web.RequestHandler):
    """JSON response handler"""

    def get(self):
        data = {
            'message': 'Hello from Tornado',
            'status': 'success'
        }
        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(data))


# ===== Path Parameters =====

class UserHandler(tornado.web.RequestHandler):
    """Handler with path parameter"""

    def get(self, user_id):
        data = {
            'user_id': user_id,
            'name': f'User {user_id}'
        }
        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(data))


# ===== Query Parameters =====

class SearchHandler(tornado.web.RequestHandler):
    """Handler with query parameters"""

    def get(self):
        query = self.get_argument('q', default='')
        page = int(self.get_argument('page', default='1'))

        data = {
            'query': query,
            'page': page,
            'results': []
        }

        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(data))


# ===== POST Requests =====

class CreateUserHandler(tornado.web.RequestHandler):
    """Handle POST requests"""

    def post(self):
        # Parse JSON body
        data = json.loads(self.request.body)

        response = {
            'message': 'User created',
            'data': data
        }

        self.set_status(201)
        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(response))


# ===== Form Handling =====

class FormHandler(tornado.web.RequestHandler):
    """Handle form submissions"""

    def get(self):
        self.write('''
            <form method="POST">
                <input type="text" name="name" placeholder="Name"><br>
                <input type="email" name="email" placeholder="Email"><br>
                <button type="submit">Submit</button>
            </form>
        ''')

    def post(self):
        name = self.get_argument('name')
        email = self.get_argument('email')

        self.write(f"<h1>Hello, {name}!</h1><p>Email: {email}</p>")


# ===== File Upload =====

class UploadHandler(tornado.web.RequestHandler):
    """Handle file uploads"""

    def post(self):
        file_data = self.request.files.get('file')[0]
        filename = file_data['filename']
        content = file_data['body']

        # Save file
        with open(f'/tmp/{filename}', 'wb') as f:
            f.write(content)

        response = {
            'message': 'File uploaded successfully',
            'filename': filename,
            'size': len(content)
        }

        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(response))


# ===== Custom Headers =====

class HeaderHandler(tornado.web.RequestHandler):
    """Read and set custom headers"""

    def get(self):
        user_agent = self.request.headers.get('User-Agent', 'Unknown')
        custom_header = self.request.headers.get('X-Custom-Header', 'None')

        self.set_header('X-Response-Header', 'Custom Value')

        data = {
            'user_agent': user_agent,
            'custom_header': custom_header
        }

        self.write(json.dumps(data))


# ===== Cookies and Sessions =====

class CookieHandler(tornado.web.RequestHandler):
    """Handle cookies"""

    def get(self):
        # Get cookie
        user_name = self.get_cookie('user_name', default='Guest')

        # Set cookie
        self.set_cookie('user_name', 'John Doe')

        self.write(f"<h1>Hello, {user_name}!</h1>")


class SecureCookieHandler(tornado.web.RequestHandler):
    """Handle secure cookies"""

    def get(self):
        # Get secure cookie
        user_id = self.get_secure_cookie('user_id')

        if user_id:
            user_id = user_id.decode('utf-8')
            self.write(f"<h1>User ID: {user_id}</h1>")
        else:
            # Set secure cookie
            self.set_secure_cookie('user_id', '12345')
            self.write("<h1>Cookie set!</h1>")


# ===== Async Handler =====

class AsyncHandler(tornado.web.RequestHandler):
    """Async request handler"""

    async def get(self):
        # Simulate async operation
        await gen.sleep(1)

        data = {
            'message': 'Async response after 1 second delay'
        }

        self.set_header("Content-Type", "application/json")
        self.write(json.dumps(data))


# ===== WebSocket Handler =====

class WebSocketHandler(tornado.websocket.WebSocketHandler):
    """WebSocket handler"""

    clients = set()

    def open(self):
        """Called when WebSocket connection is opened"""
        WebSocketHandler.clients.add(self)
        print(f"WebSocket opened. Total clients: {len(WebSocketHandler.clients)}")

    def on_message(self, message):
        """Called when message is received"""
        print(f"Received: {message}")

        # Echo message back
        self.write_message(f"Echo: {message}")

        # Broadcast to all clients
        # for client in WebSocketHandler.clients:
        #     client.write_message(message)

    def on_close(self):
        """Called when WebSocket connection is closed"""
        WebSocketHandler.clients.discard(self)
        print(f"WebSocket closed. Total clients: {len(WebSocketHandler.clients)}")

    def check_origin(self, origin):
        """Allow all origins (customize for production)"""
        return True


# ===== Error Handler =====

class NotFoundHandler(tornado.web.RequestHandler):
    """Custom 404 handler"""

    def prepare(self):
        self.set_status(404)
        self.write({
            'error': 'Resource not found',
            'status': 404
        })


# ===== Base Handler with Shared Logic =====

class BaseHandler(tornado.web.RequestHandler):
    """Base handler with shared methods"""

    def set_default_headers(self):
        """Add default headers to all responses"""
        self.set_header("X-Custom-Header", "Tornado App")

    def write_error(self, status_code, **kwargs):
        """Custom error response"""
        self.set_header("Content-Type", "application/json")
        self.write({
            'error': self.reason,
            'status': status_code
        })


class APIHandler(BaseHandler):
    """API handler inheriting from BaseHandler"""

    def get(self):
        data = {
            'message': 'API response with base handler',
            'status': 'success'
        }
        self.write(json.dumps(data))


# ===== Application Configuration =====

def make_app():
    """Create and configure the application"""
    return tornado.web.Application(
        [
            (r"/", MainHandler),
            (r"/api/hello", JSONHandler),
            (r"/api/users/([0-9]+)", UserHandler),
            (r"/api/search", SearchHandler),
            (r"/api/users", CreateUserHandler),
            (r"/form", FormHandler),
            (r"/upload", UploadHandler),
            (r"/headers", HeaderHandler),
            (r"/cookie", CookieHandler),
            (r"/secure-cookie", SecureCookieHandler),
            (r"/async", AsyncHandler),
            (r"/ws", WebSocketHandler),
            (r"/api/v2/status", APIHandler),
        ],
        # Application settings
        cookie_secret="__TODO:_GENERATE_YOUR_OWN_RANDOM_VALUE_HERE__",
        debug=True,  # Auto-reload on code changes
        autoreload=True,
        default_handler_class=NotFoundHandler
    )


# ===== Long Polling Example =====

class LongPollingHandler(tornado.web.RequestHandler):
    """Long polling handler"""

    waiters = set()
    messages = []

    async def get(self):
        """Long polling GET request"""
        self.set_header("Content-Type", "application/json")

        # Wait for new message
        future = tornado.concurrent.Future()
        LongPollingHandler.waiters.add(future)

        try:
            # Wait up to 30 seconds for a message
            message = await gen.with_timeout(
                tornado.ioloop.IOLoop.current().time() + 30,
                future
            )
            self.write(json.dumps({'message': message}))
        except gen.TimeoutError:
            self.write(json.dumps({'message': 'No new messages'}))
        finally:
            LongPollingHandler.waiters.discard(future)

    def post(self):
        """Post new message"""
        message = json.loads(self.request.body)['message']

        # Notify all waiting clients
        for future in LongPollingHandler.waiters:
            future.set_result(message)

        LongPollingHandler.waiters.clear()
        self.write(json.dumps({'status': 'Message sent'}))


if __name__ == "__main__":
    print("Tornado Web Framework Example")
    print("\nFeatures demonstrated:")
    print("  - Basic request handlers")
    print("  - Path and query parameters")
    print("  - JSON API endpoints")
    print("  - Form handling")
    print("  - File uploads")
    print("  - Cookies (regular and secure)")
    print("  - Async handlers")
    print("  - WebSocket support")
    print("  - Custom error handlers")
    print("\nTo run this server:")
    print("  python tornado_example.py")
    print("\nThen visit:")
    print("  http://localhost:8888/")
    print("  http://localhost:8888/api/hello")
    print("  http://localhost:8888/api/users/123")
    print("  http://localhost:8888/form")
    print("\nWebSocket test:")
    print("  ws://localhost:8888/ws")

    # Uncomment to run the server
    # app = make_app()
    # app.listen(8888)
    # print("\nServer running on http://localhost:8888")
    # tornado.ioloop.IOLoop.current().start()
