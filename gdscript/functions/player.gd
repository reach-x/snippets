extends CharacterBody2D

# GDScript example for Godot Engine

const SPEED = 300.0
const JUMP_VELOCITY = -400.0

@export var health: int = 100
@export var max_health: int = 100

var gravity = ProjectSettings.get_setting("physics/2d/default_gravity")
var is_attacking = false

@onready var sprite = $Sprite2D
@onready var animation_player = $AnimationPlayer

func _ready():
    print("Player initialized")

func _physics_process(delta):
    if not is_on_floor():
        velocity.y += gravity * delta

    if Input.is_action_just_pressed("jump") and is_on_floor():
        velocity.y = JUMP_VELOCITY

    var direction = Input.get_axis("move_left", "move_right")
    if direction:
        velocity.x = direction * SPEED
        sprite.flip_h = direction < 0
    else:
        velocity.x = move_toward(velocity.x, 0, SPEED)

    move_and_slide()

func take_damage(amount: int):
    health -= amount
    if health <= 0:
        die()

func die():
    queue_free()
