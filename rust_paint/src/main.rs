use bevy::{prelude::*, sprite::MaterialMesh2dBundle};
use bevy::render::camera::RenderTarget;
use bevy::window::*;

#[derive(Component)]
struct Person;

#[derive(Component)]
struct Name(String);

#[derive(Component)]
struct GreetTimer(Timer);

#[derive(Component)]
struct Circle;

#[derive(Component)]
struct MainCam;

#[derive(Default)]
struct All {
    col: Color,
    rand: bool
}

fn add_people(mut commands: Commands) {
    commands.spawn().insert(Person).insert(Name("Elaina Proctor".to_string()));
    commands.spawn().insert(Person).insert(Name("Renzo Hume".to_string()));
    commands.spawn().insert(Person).insert(Name("Zayna Nieves".to_string()));
}

fn greet_people(time: Res<Time>, mut timer: ResMut<GreetTimer>, query: Query<&Name, With<Person>>) {
    if timer.0.tick(time.delta()).just_finished() {
        for name in query.iter() {
            println!("hello {}!", name.0);
        }
    }
}

pub struct HelloPlugin;

impl Plugin for HelloPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(GreetTimer(Timer::from_seconds(2.0, true)))
        .add_startup_system(add_people)
        .add_system(greet_people);
    }
}

fn spawn_circle (mut commands: Commands, mut meshes: ResMut<Assets<Mesh>>, buttons: Res<Input<MouseButton>>, mut materials: ResMut<Assets<ColorMaterial>>, wnds: Res<Windows>, q_camera: Query<(&Camera, &GlobalTransform)>, col: Res<All>)
{
    if !buttons.pressed(MouseButton::Left) {
        return;
    }
    let (camera, camera_transform) = q_camera.single();
    let wnd = if let RenderTarget::Window(id) = camera.target {
        wnds.get(id).unwrap()
    } else {
        wnds.get_primary().unwrap()
    };
    if let Some(screen_pos) = wnd.cursor_position() {
        let window_size = Vec2::new(wnd.width() as f32, wnd.height() as f32);
        let ndc = (screen_pos / window_size) * 2.0 - Vec2::ONE;
        let ndc_to_world = camera_transform.compute_matrix() * camera.projection_matrix().inverse();
        let world_pos = ndc_to_world.project_point3(ndc.extend(-1.0));
        let world_pos: Vec2 = world_pos.truncate();
        commands.spawn_bundle(MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::new(50.).into()).into(),
            material: materials.add(ColorMaterial::from(col.col)),
            transform: Transform::from_translation(Vec3::new(world_pos.x, world_pos.y, 0.)),
            ..default()
        })
        .insert(Circle);
    }
}

fn setup(
    mut commands: Commands
) {
    commands.spawn_bundle(Camera2dBundle::default())
        .insert(MainCam);
}

fn change_color(keyc: Res<Input<KeyCode>>, mut col: ResMut<All>) {
    if keyc.just_pressed(KeyCode::R) {
        col.rand = false;
        col.col = Color::RED;
    }
    if keyc.just_pressed(KeyCode::G) {
        col.rand = false;
        col.col = Color::GREEN;
    }
    if keyc.just_pressed(KeyCode::B) {
        col.rand = false;
        col.col = Color::BLUE;
    }
    if keyc.just_pressed(KeyCode::NumpadMultiply) {
        col.rand = true;
    }
    if col.rand {
        col.col = Color::rgb(rand::random::<f32>(), rand::random::<f32>(), rand::random::<f32>());
    }
}

fn remove_all_circle(mut commands: Commands, q_circle: Query<Entity, With<Circle>>, keyc: Res<Input<KeyCode>>) {
    if keyc.just_pressed(KeyCode::F) {
        for entity in q_circle.iter() {
            commands.entity(entity).despawn();
        }
    }
}

// fn cursor_events(
//     app: &mut App,
//     mut cursor_evr: EventReader<CursorMoved>,
// ) {
//     for ev in cursor_evr.iter() {
//     }
// }

fn main() {
    App::new()
        .insert_resource(WindowDescriptor {
            mode: WindowMode::Fullscreen,
            ..default()
        })
        .add_plugins(DefaultPlugins)
        .add_system(bevy::window::close_on_esc)
        .insert_resource(All{col: Color::RED, rand: false})
        .add_startup_system(setup)
        .add_system(change_color)
        .add_system(remove_all_circle)
        .add_system(spawn_circle)
        .run();
}