use crate::use_controlled;
use dioxus::html::geometry::{ClientPoint, ElementPoint};
use dioxus_lib::html::geometry::Pixels;
use dioxus_lib::html::geometry::euclid::Rect;
use dioxus_lib::prelude::*;
use std::ops::RangeInclusive;

#[derive(Debug, Clone, PartialEq)]
pub enum SliderValue {
    Single(f64),
    Range(f64, f64),
}

impl std::fmt::Display for SliderValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SliderValue::Single(v) => write!(f, "{}", v),
            SliderValue::Range(start, end) => write!(f, "{}, {}", start, end),
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SliderProps {
    /// The controlled value of the slider
    value: Option<Signal<SliderValue>>,

    /// The default value when uncontrolled
    #[props(default = SliderValue::Single(0.0))]
    default_value: SliderValue,

    /// The minimum value
    #[props(default = 0.0)]
    min: f64,

    /// The maximum value
    #[props(default = 100.0)]
    max: f64,

    /// The step value
    #[props(default = 1.0)]
    step: f64,

    /// Whether the slider is disabled
    #[props(default)]
    disabled: ReadOnlySignal<bool>,

    /// Orientation of the slider
    #[props(default = true)]
    horizontal: bool,

    /// Inverts the order of the values
    #[props(default)]
    inverted: bool,

    /// Callback when value changes
    #[props(default)]
    on_value_change: Callback<SliderValue>,

    #[props(extends = GlobalAttributes)]
    attributes: Vec<Attribute>,

    children: Element,
}

#[component]
pub fn Slider(props: SliderProps) -> Element {
    let (value, set_value) = use_controlled(
        props.value,
        props.default_value.clone(),
        props.on_value_change,
    );

    let orientation = if props.horizontal {
        "horizontal"
    } else {
        "vertical"
    };
    let range = props.min..=props.max;

    let mut dragging = use_signal(|| false);

    let ctx = use_context_provider(|| SliderContext {
        value,
        set_value,
        min: props.min,
        max: props.max,
        step: props.step,
        disabled: props.disabled,
        horizontal: props.horizontal,
        inverted: props.inverted,
        range,
        dragging: dragging.into(),
    });

    let mut rect = use_signal(|| None);
    let mut div_element = use_signal(|| None);
    let mut last_mouse_position: Signal<Option<ClientPoint>> = use_signal(|| None);
    let mut granular_value = use_signal(|| {
        props.default_value.clone()
    });

    let size = rect().map(|r: Rect<f64, Pixels>| {
        if props.horizontal {
            r.width()
        } else {
            r.height()
        }
    });

    let mut update_position = move |e: &Event<MouseData>| {
        let Some(size) = size else {
            tracing::warn!("Slider size is not (yet) set");
            return;
        };
        let client_coordinates = e.data().client_coordinates();

        let Some(current_last_mouse_position) = last_mouse_position() else {
            return;
        };
        let delta = client_coordinates - current_last_mouse_position;

        let delta_pos = if ctx.horizontal {
            delta.x
        } else {
            delta.y
        } as f64;

        let delta = get_value_from_pointer(delta_pos, size, ctx.min, ctx.max, ctx.inverted);

        let current_value = match granular_value() {
            SliderValue::Single(v) => v,
            SliderValue::Range(start, _) => {
                // TODO: Handle range sliders
                start
            }
        };
        let stepped = ((current_value + delta) / ctx.step).round() * ctx.step;
        ctx.set_value.call(SliderValue::Single(stepped));
        granular_value.set(SliderValue::Single(stepped));
    };

    rsx! {
        div {
            role: "group",
            "data-disabled": props.disabled,
            "data-orientation": orientation,

            onmounted: move |evt| async move {
                // Get the bounding rect of the slider
                if let Ok(r) = evt.data().get_client_rect().await {
                    rect.set(Some(r));
                }
                div_element.set(Some(evt.data()));
            },
            onresize: move |_| async move {
                // Update the rect on resize
                let Some(div_element) = div_element() else {
                    tracing::warn!("Slider div element is not (yet) set");
                    return;
                };
                if let Ok(r) = div_element.get_client_rect().await {
                    tracing::info!("Slider resized: {:?}", r);
                    rect.set(Some(r));
                }
            },
            onmousemove: move |e| {
                if !dragging() || (ctx.disabled)() {
                    return;
                }

                update_position(&e);
                last_mouse_position.set(Some(e.data().client_coordinates()));
            },

            onmousedown: move |e| {
                if (ctx.disabled)() {
                    return;
                }

                dragging.set(true);
                update_position(&e);
            },

            onmouseup: move |_| {
                dragging.set(false);
                last_mouse_position.take();
            },
            ..props.attributes,

            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SliderTrackProps {
    #[props(extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
    children: Element,
}

#[component]
pub fn SliderTrack(props: SliderTrackProps) -> Element {
    let ctx = use_context::<SliderContext>();
    let orientation = if ctx.horizontal {
        "horizontal"
    } else {
        "vertical"
    };

    rsx! {
        div {
            "data-disabled": ctx.disabled,
            "data-orientation": orientation,
            ..props.attributes,
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SliderRangeProps {
    #[props(extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
}

#[component]
pub fn SliderRange(props: SliderRangeProps) -> Element {
    let ctx = use_context::<SliderContext>();
    let orientation = if ctx.horizontal {
        "horizontal"
    } else {
        "vertical"
    };

    let style = use_memo(move || {
        let (start, end) = match (ctx.value)() {
            SliderValue::Single(v) => (ctx.min, v),
            SliderValue::Range(start, end) => (start, end),
        };

        let start_percent = ((start - ctx.min) / (ctx.max - ctx.min) * 100.0).clamp(0.0, 100.0);
        let end_percent = ((end - ctx.min) / (ctx.max - ctx.min) * 100.0).clamp(0.0, 100.0);

        if ctx.horizontal {
            format!("left: {}%; right: {}%", start_percent, 100.0 - end_percent)
        } else {
            format!("bottom: {}%; top: {}%", start_percent, 100.0 - end_percent)
        }
    });

    rsx! {
        div {
            "data-disabled": ctx.disabled,
            "data-orientation": orientation,
            style,
            ..props.attributes,
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SliderThumbProps {
    /// Which thumb this is in a range slider
    #[props(default)]
    index: Option<usize>,

    #[props(extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
}

#[component]
pub fn SliderThumb(props: SliderThumbProps) -> Element {
    let ctx = use_context::<SliderContext>();
    let orientation = if ctx.horizontal {
        "horizontal"
    } else {
        "vertical"
    };

    let value = use_memo(move || match ((ctx.value)(), props.index) {
        (SliderValue::Single(v), _) => v,
        (SliderValue::Range(start, _), Some(0)) => start,
        (SliderValue::Range(_, end), Some(1)) => end,
        _ => ctx.min,
    });

    let percent = ((value() - ctx.min) / (ctx.max - ctx.min) * 100.0).clamp(0.0, 100.0);
    let style = if ctx.horizontal {
        format!("left: {}%", percent)
    } else {
        format!("bottom: {}%", percent)
    };

    rsx! {
        button {
            r#type: "button",
            role: "slider",
            aria_valuemin: ctx.min,
            aria_valuemax: ctx.max,
            aria_valuenow: value,
            aria_orientation: orientation,
            "data-disabled": ctx.disabled,
            "data-orientation": orientation,
            "data-dragging": ctx.dragging,
            style,
            tabindex: 0,
            ..props.attributes,
        }
    }
}

/// Performs a linear scale transformation between two ranges.
///
/// # Arguments
///
/// * `input` - Input range [min, max]
/// * `output` - Output range [min, max]
///
/// # Returns
///
/// A function that maps values from the input range to the output range
fn linear_scale(input: [f64; 2], output: [f64; 2]) -> impl Fn(f64) -> f64 {
    let [in_min, in_max] = input;
    let [out_min, out_max] = output;

    move |x: f64| {
        // Calculate position in input range (0.0 ~ 1.0)
        let normalized = (x - in_min) / (in_max - in_min);

        // Convert to output range
        out_min + normalized * (out_max - out_min)
    }
}

/// Calculates a value based on pointer position within a rectangle.
///
/// # Arguments
///
/// * `pointer_position` - The position of the pointer
/// * `size` - The rectangle width (or height for vertical sliders)
/// * `min` - The minimum value in the output range
/// * `max` - The maximum value in the output range
/// * `inverted` - Whether to invert the output range
///
/// # Returns
///
/// The calculated value within the range
fn get_value_from_pointer(
    pointer_position: f64,
    size: f64,
    min: f64,
    max: f64,
    inverted: bool,
) -> f64 {
    let input = [0.0, size];
    let output = if !inverted { [min, max] } else { [max, min] };
    let value = linear_scale(input, output);

    value(pointer_position)
}

#[allow(dead_code)]
#[derive(Clone)]
struct SliderContext {
    value: Memo<SliderValue>,
    set_value: Callback<SliderValue>,
    min: f64,
    max: f64,
    step: f64,
    disabled: ReadOnlySignal<bool>,
    horizontal: bool,
    inverted: bool,
    range: RangeInclusive<f64>,
    dragging: ReadOnlySignal<bool>,
}
