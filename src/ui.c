#include "ui.h"

#include "window.h"
#include "shapes.h"
#include "text.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

struct UIElementFixed
{
    struct UIElement *element;
    int              x;
    int              y;
};

struct UIElementArray
{
    int              elements_length;
    struct UIElement **elements;
    int              *elements_sizes;
};

struct UIElementBox
{
    unsigned int     VAO;
    unsigned int     VBO;
    struct RGBA      color;
    struct UIElement *element;
};

struct UIElementText
{
    unsigned int VAO;
    unsigned int VBO;
    unsigned int TID;
    struct RGBA  color;
    const char   *text;
    int          texture_width;
    int          texture_height;
};

enum UIElementType
{
    UIELEMENT_BORDER,
    UIELEMENT_FIXED,
    UIELEMENT_VERTICAL,
    UIELEMENT_HORIZONTAL,
    UIELEMENT_TEXT,
    UIELEMENT_BOX
};

union UIElementValue
{
    struct UIElementArray array;
    struct UIElementText  text;
    struct UIElementBox   box;
    struct UIElementFixed fixed;
};

struct UIElement
{
    enum UIElementType   type;
    union UIElementValue value;
};

struct UIElement *uielement_border(struct UIElement* element, int x, int y)
{
    struct UIElement *e    = malloc(sizeof(struct UIElement));
    e->type                = UIELEMENT_BORDER;
    e->value.fixed.element = element;
    e->value.fixed.x       = x;
    e->value.fixed.y       = y;
    return e;
}

struct UIElement *uielement_fixed(struct UIElement* element, int x, int y)
{
    struct UIElement *e    = malloc(sizeof(struct UIElement));
    e->type                = UIELEMENT_FIXED;
    e->value.fixed.element = element;
    e->value.fixed.x       = x;
    e->value.fixed.y       = y;
    return e;
}

struct UIElement *uielement_array(va_list va, int length)
{
    struct UIElement *e              = malloc(sizeof(struct UIElement));
    struct UIElement **elements      = malloc(sizeof(struct UIElement[length]));
    int              *elements_sizes = malloc(sizeof(int[length]));
    for (int i = 0; i < length; i++)
    {
        elements[i]       = va_arg(va, struct UIElement*);
        elements_sizes[i] = va_arg(va, int);
    }
    e->value.array =
        (struct UIElementArray)
            {.elements_length = length,
             .elements        = elements,
             .elements_sizes  = elements_sizes};
    return e;
}

struct UIElement *uielement_vertical(int length, ...)
{
    va_list va;
    va_start(va, length);
    struct UIElement *e = uielement_array(va, length);
    va_end(va);

    e->type = UIELEMENT_VERTICAL;
    return e;
}

struct UIElement *uielement_horizontal(int length, ...)
{
    va_list va;
    va_start(va, length);
    struct UIElement *e = uielement_array(va, length);
    va_end(va);

    e->type = UIELEMENT_HORIZONTAL;
    return e;
}

struct UIElement *uielement_text(struct RGBA color, const char* text)
{
    struct UIElement *e = malloc(sizeof(struct UIElement));
    e->type             = UIELEMENT_TEXT;
    e->value.text.color = color;
    e->value.text.text  = text;

    char *texture;
    textRender(e->value.text.text, &texture, &e->value.text.texture_width, &e->value.text.texture_height);

    shapesGenerateText(0,0,0,0,0,(struct RGBA){0,0,0,0},
                       texture,
                       e->value.text.texture_width,
                       e->value.text.texture_height,
                       &e->value.text.TID,
                       &e->value.text.VAO,
                       &e->value.text.VBO);
    return e;
}

struct UIElement *uielement_box(struct RGBA color, struct UIElement* element)
{
    struct UIElement *e  = malloc(sizeof(struct UIElement));
    e->type              = UIELEMENT_BOX;
    e->value.box.color   = color;
    e->value.box.element = element;
    shapesGenerateBox(0,0,0,0,0,(struct RGBA){0,0,0,0},
                      &e->value.box.VAO,&e->value.box.VBO);
    return e;
}

int uielement_max_layer_partial(struct UIElement *element, int layer)
{
    if (element == NULL) return layer;
    switch(element->type)
    {
        case UIELEMENT_BORDER:
        case UIELEMENT_FIXED:
            return uielement_max_layer_partial(element->value.fixed.element, layer+1);
            break;
        case UIELEMENT_VERTICAL:
        case UIELEMENT_HORIZONTAL:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            int max_layer = layer;
            for(int i = 0; i < element->value.array.elements_length; i++)
            {
                int max_layer_i = uielement_max_layer_partial(element->value.array.elements[i], layer+1);
                if (max_layer_i > max_layer) max_layer = max_layer_i;
            }
            return max_layer;
            break;
        case UIELEMENT_TEXT:
            return layer;
            break;
        case UIELEMENT_BOX:
            return uielement_max_layer_partial(element->value.box.element, layer+1);
            break;
    }
}

int uielement_max_layer(struct UIElement *element)
{
    return uielement_max_layer_partial(element, 0);
}

void uielement_update_partial(struct UIElement *element,
                                float upper_x, float lower_x,
                                float upper_y, float lower_y,
                                int layer, int max_layer)
{
    if (element == NULL) return;
    switch (element->type)
    {
        case UIELEMENT_BORDER:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float fixed_x = (float) element->value.fixed.x / (float) windowX;
            float fixed_y = (float) element->value.fixed.y / (float) windowY;
            uielement_update_partial(element->value.fixed.element,
                                     upper_x - fixed_x, lower_x + fixed_x,
                                     upper_y - fixed_y, lower_y + fixed_y,
                                     layer + 1, max_layer);
            break;
        case UIELEMENT_FIXED:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float center_x = (upper_x + lower_x) / 2;
            float center_y = (upper_y + lower_y) / 2 ;
            fixed_x = (float) element->value.fixed.x / (float) windowX;
            fixed_y = (float) element->value.fixed.y / (float) windowY;
            uielement_update_partial(element->value.fixed.element,
                                     center_x + fixed_x, center_x - fixed_x,
                                     center_y + fixed_y, center_y - fixed_y,
                                     layer + 1, max_layer);
            break;
        case UIELEMENT_VERTICAL:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float total_size = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                total_size += element->value.array.elements_sizes[i];
            }
            float specific_bound = (upper_y - lower_y)/total_size;
            float current_bound = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_update_partial(element->value.array.elements[i],
                                         upper_x, lower_x,
                                         lower_y + current_bound + (specific_bound * element->value.array.elements_sizes[i]),
                                         lower_y + current_bound,
                                         layer + 1, max_layer);
                current_bound += specific_bound * element->value.array.elements_sizes[i];
            }
            break;
        case UIELEMENT_HORIZONTAL:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            total_size = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                total_size += element->value.array.elements_sizes[i];
            }
            specific_bound = (upper_x - lower_x)/total_size;
            current_bound = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_update_partial(element->value.array.elements[i],
                                         lower_x + current_bound + (specific_bound * element->value.array.elements_sizes[i]),
                                         lower_x + current_bound,
                                         upper_y, lower_y, layer + 1, max_layer);
                current_bound += specific_bound * element->value.array.elements_sizes[i];
            }
            break;
        case UIELEMENT_TEXT:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            center_x = (upper_x  + lower_x) / 2;
            center_y = (upper_y + lower_y) / 2;
            float offset_x = (float) element->value.text.texture_width / (float) windowX;
            float offset_y = (float) element->value.text.texture_height / (float) windowY;
            shapesUpdateText(center_x + offset_x, center_x - offset_x,
                             center_y + offset_y, center_y - offset_y,
                             (float) layer / (float) max_layer,
                             element->value.text.color,
                             &element->value.text.VBO);
            break;
        case UIELEMENT_BOX:
            shapesUpdateBox(upper_x, lower_x, upper_y, lower_y,
                            (float) layer / (float) max_layer, element->value.box.color,
                            &element->value.box.VBO);
            uielement_update_partial(element->value.box.element,
                                       upper_x, lower_x, upper_y, lower_y,
                                       layer, max_layer);
            break;
    }
}

void uielement_update(struct UIElement *element)
{
    uielement_update_partial(element, 1.0, -1.0, 1.0, -1.0, 0, uielement_max_layer(element));
}

void uielement_draw(struct UIElement *element)
{
    if (element == NULL) return;
    switch (element->type)
    {
        case UIELEMENT_BORDER:
        case UIELEMENT_FIXED:
            uielement_draw(element->value.fixed.element);
            break;
        case UIELEMENT_VERTICAL:
        case UIELEMENT_HORIZONTAL:
            for(int i = 0; i < element->value.array.elements_length; i++)
                uielement_draw(element->value.array.elements[i]);
            break;
        case UIELEMENT_TEXT:
            shapesDrawText(&element->value.text.TID, &element->value.text.VAO);
            break;
        case UIELEMENT_BOX:
            shapesDrawBox(&element->value.box.VAO);
            uielement_draw(element->value.box.element);
            break;
    }
}

void uielement_destructor(struct UIElement *element)
{
    if (element == NULL) return;
    switch (element->type)
    {
        case UIELEMENT_BORDER:
        case UIELEMENT_FIXED:
            uielement_destructor(element->value.fixed.element);
            break;
        case UIELEMENT_VERTICAL:
        case UIELEMENT_HORIZONTAL:
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_destructor(element->value.array.elements[i]);
                free(element->value.array.elements);
                free(element->value.array.elements_sizes);
            }
            break;
        case UIELEMENT_BOX:
            uielement_destructor(element->value.box.element);
            break;
    }
    free(element);
}
