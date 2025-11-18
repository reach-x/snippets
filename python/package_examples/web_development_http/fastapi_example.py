"""
fastapi - Modern async API framework
Install: pip install fastapi uvicorn

High-performance framework with auto docs, type hints, and validation
"""

from fastapi import FastAPI, HTTPException, Query, Path, Header, Depends, status, File, UploadFile
from fastapi.responses import JSONResponse
from pydantic import BaseModel, EmailStr, validator
from typing import Optional, List
from enum import Enum

app = FastAPI(
    title="FastAPI Example",
    description="Comprehensive FastAPI examples",
    version="1.0.0"
)

# In-memory databases
users_database = {}
items_database = []


# ===== Pydantic Models (Data Validation) =====

class User(BaseModel):
    """User model with automatic validation"""
    id: Optional[int] = None
    name: str
    email: EmailStr
    age: Optional[int] = None
    is_active: bool = True

    @validator('age')
    def validate_age(cls, value):
        if value is not None and (value < 0 or value > 150):
            raise ValueError('Age must be between 0 and 150')
        return value


class UserUpdate(BaseModel):
    """Model for updating user (all fields optional)"""
    name: Optional[str] = None
    email: Optional[EmailStr] = None
    age: Optional[int] = None
    is_active: Optional[bool] = None


class Item(BaseModel):
    """Item model"""
    name: str
    description: Optional[str] = None
    price: float
    tax: Optional[float] = None


class ItemCategory(str, Enum):
    """Enum for item categories"""
    ELECTRONICS = "electronics"
    CLOTHING = "clothing"
    FOOD = "food"


# ===== Basic Routes =====

@app.get("/")
async def root():
    """Root endpoint"""
    return {"message": "Welcome to FastAPI", "docs": "/docs"}


@app.get("/hello/{name}")
async def hello_name(name: str):
    """Path parameter"""
    return {"message": f"Hello, {name}!"}


# ===== CRUD Operations with Type Hints =====

@app.get("/users", response_model=List[User])
async def get_users():
    """Get all users - auto validated response"""
    return list(users_database.values())


@app.post("/users", response_model=User, status_code=status.HTTP_201_CREATED)
async def create_user(user: User):
    """Create user - auto validated input"""
    user.id = len(users_database) + 1
    users_database[user.id] = user
    return user


@app.get("/users/{user_id}", response_model=User)
async def get_user(
    user_id: int = Path(..., gt=0, description="The ID of the user to get")
):
    """Get user by ID with path validation"""
    if user_id not in users_database:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"User with id {user_id} not found"
        )

    return users_database[user_id]


@app.put("/users/{user_id}", response_model=User)
async def update_user(user_id: int, user_update: UserUpdate):
    """Update user"""
    if user_id not in users_database:
        raise HTTPException(status_code=404, detail="User not found")

    user = users_database[user_id]
    update_data = user_update.dict(exclude_unset=True)

    updated_user = user.copy(update=update_data)
    users_database[user_id] = updated_user

    return updated_user


@app.delete("/users/{user_id}")
async def delete_user(user_id: int):
    """Delete user"""
    if user_id not in users_database:
        raise HTTPException(status_code=404, detail="User not found")

    del users_database[user_id]
    return {"message": "User deleted successfully"}


# ===== Query Parameters with Validation =====

@app.get("/items")
async def search_items(
    query: str = Query(..., min_length=1, max_length=50),
    skip: int = Query(0, ge=0),
    limit: int = Query(10, ge=1, le=100),
    category: Optional[ItemCategory] = None
):
    """Search items with query parameter validation"""
    return {
        "query": query,
        "skip": skip,
        "limit": limit,
        "category": category,
        "results": items_database[skip:skip + limit]
    }


# ===== Request Headers =====

@app.get("/headers")
async def read_headers(
    user_agent: Optional[str] = Header(None),
    x_api_key: Optional[str] = Header(None)
):
    """Read request headers"""
    return {
        "user_agent": user_agent,
        "api_key": x_api_key
    }


# ===== Dependency Injection =====

async def verify_api_key(x_api_key: str = Header(...)):
    """Dependency: Verify API key"""
    if x_api_key != "secret-key":
        raise HTTPException(status_code=401, detail="Invalid API key")
    return x_api_key


async def get_current_user(user_id: int = Header(...)):
    """Dependency: Get current user from header"""
    if user_id not in users_database:
        raise HTTPException(status_code=404, detail="User not found")
    return users_database[user_id]


@app.get("/protected")
async def protected_route(api_key: str = Depends(verify_api_key)):
    """Protected route using dependency"""
    return {"message": "Access granted", "api_key": api_key}


@app.get("/me", response_model=User)
async def get_my_profile(current_user: User = Depends(get_current_user)):
    """Get current user profile using dependency"""
    return current_user


# ===== File Upload =====

@app.post("/upload")
async def upload_file(file: UploadFile = File(...)):
    """Upload file"""
    contents = await file.read()

    return {
        "filename": file.filename,
        "content_type": file.content_type,
        "size": len(contents)
    }


@app.post("/upload-multiple")
async def upload_multiple_files(files: List[UploadFile] = File(...)):
    """Upload multiple files"""
    return {
        "files": [
            {
                "filename": file.filename,
                "content_type": file.content_type
            }
            for file in files
        ]
    }


# ===== Response Models and Status Codes =====

class ResponseMessage(BaseModel):
    """Standard response model"""
    message: str
    status: str = "success"


@app.post("/items", response_model=ResponseMessage, status_code=201)
async def create_item(item: Item):
    """Create item with specific response model and status code"""
    items_database.append(item)
    return ResponseMessage(message="Item created successfully")


# ===== Custom Response =====

@app.get("/custom-response")
async def custom_response():
    """Return custom JSON response"""
    return JSONResponse(
        content={"message": "Custom response"},
        status_code=200,
        headers={"X-Custom-Header": "Value"}
    )


# ===== Background Tasks =====

from fastapi import BackgroundTasks

def write_log(message: str):
    """Background task"""
    with open("/tmp/fastapi_log.txt", "a") as log_file:
        log_file.write(f"{message}\n")


@app.post("/send-notification")
async def send_notification(
    email: EmailStr,
    background_tasks: BackgroundTasks
):
    """Add background task"""
    background_tasks.add_task(write_log, f"Notification sent to {email}")
    return {"message": "Notification sent"}


# ===== Exception Handlers =====

@app.exception_handler(ValueError)
async def value_error_handler(request, exc):
    """Custom exception handler"""
    return JSONResponse(
        status_code=400,
        content={"message": str(exc)}
    )


# ===== Middleware =====

@app.middleware("http")
async def add_process_time_header(request, call_next):
    """Add custom header to all responses"""
    import time
    start_time = time.time()

    response = await call_next(request)

    process_time = time.time() - start_time
    response.headers["X-Process-Time"] = str(process_time)

    return response


# ===== Startup/Shutdown Events =====

@app.on_event("startup")
async def startup_event():
    """Run on application startup"""
    print("FastAPI application starting up...")
    # Initialize database connections, load models, etc.


@app.on_event("shutdown")
async def shutdown_event():
    """Run on application shutdown"""
    print("FastAPI application shutting down...")
    # Close database connections, cleanup, etc.


if __name__ == "__main__":
    print("FastAPI Example Application")
    print("\nTo run this application:")
    print("  pip install fastapi uvicorn")
    print("  uvicorn fastapi_example:app --reload")
    print("\nThen visit:")
    print("  http://localhost:8000          - API")
    print("  http://localhost:8000/docs     - Interactive API docs (Swagger UI)")
    print("  http://localhost:8000/redoc    - Alternative docs (ReDoc)")
    print("\nFeatures demonstrated:")
    print("  - Automatic request validation with Pydantic")
    print("  - Type hints for all parameters")
    print("  - Auto-generated interactive documentation")
    print("  - Dependency injection")
    print("  - File uploads")
    print("  - Background tasks")
    print("  - Custom middleware")
    print("  - Response models")
