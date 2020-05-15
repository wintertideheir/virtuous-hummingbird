#include "text.h"

#include "error.h"

#include <stdlib.h>
#include <string.h>
#include <ft2build.h>
#include FT_FREETYPE_H

struct TextCharacter
{
    unsigned long charcode;
    int           width;
    int           height;
    char *        bitmap;
    int           x_offset;
    int           y_offset;
    long          advance;
};

const char* FACE_PATH        = "/usr/share/fonts/truetype/msttcorefonts/times.ttf";
const unsigned int FACE_SIZE = 32;

FT_Library library;
FT_Face    face;

int                  characters_length;
struct TextCharacter *characters;

void textBegin()
{
    int freetype_status = FT_Init_FreeType(&library);
    if (freetype_status != 0)
    {
        errorReport("FreeType2 failed to initialize (Error Code %i).\n", freetype_status);
        errorExit();
    }

    freetype_status = FT_New_Face(library, FACE_PATH, 0, &face);
    if (freetype_status != 0)
    {
        errorReport("FreeType2 failed to load %s (Error Code %i).\n", FACE_PATH, freetype_status);
        errorExit();
    }

    freetype_status = FT_Set_Pixel_Sizes(face, 0, FACE_SIZE);
    if (freetype_status != 0)
    {
        errorReport("FreeType2 failed to set the pixel size %u (Error Code %i) (Face Path %s).\n",
                    (unsigned int) FACE_SIZE, freetype_status, FACE_PATH);
        errorExit();
    }

    characters_length = 0;
    characters        = calloc(1, sizeof(struct TextCharacter));
}

struct TextCharacter* textCharacter(unsigned long charcode)
{
    for (int i = 0; i < characters_length; i++)
    {
        if (characters[i].charcode == charcode)
        {
            return &characters[i];
        }
    }

    unsigned long glyph = FT_Get_Char_Index(face, charcode);
    if (glyph == 0)
    {
        errorReport("FreeType2 failed to retrieve the glyph index for the character %lu"
                    " (Error Code %i) (Face Path %s).\n", charcode, FACE_PATH);
        errorExit();
    }

    int freetype_status = FT_Load_Glyph(face, glyph, FT_LOAD_DEFAULT);
    if (freetype_status != 0)
    {
        errorReport("FreeType2 failed to load the glyph %ul (Error Code %i). \n", glyph, freetype_status);
        errorExit();
    }

    freetype_status = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
    if (freetype_status != 0)
    {
        errorReport("FreeType2 failed to render the glyph %ul (Error Code %i). \n", glyph, freetype_status);
        errorExit();
    }

    characters_length++;
    characters = realloc(characters, characters_length * sizeof(struct TextCharacter));
    int bitmap_size = sizeof(char[face->glyph->bitmap.rows][face->glyph->bitmap.width]);
    char *bitmap = malloc(bitmap_size);
    memcpy(bitmap, face->glyph->bitmap.buffer, bitmap_size);
    characters[characters_length - 1] = (struct TextCharacter)
    {
        .bitmap    = bitmap,
        .charcode  = charcode,
        .width     = face->glyph->bitmap.width,
        .height    = face->glyph->bitmap.rows,
        .x_offset  = face->glyph->bitmap_left,
        .y_offset  = face->glyph->bitmap_top,
        .advance   = face->glyph->advance.x / 64,
    };
    return &characters[characters_length - 1];
}

void textRender(const char* text, char** texture,
                int* texture_width, int* texture_height)
{
    int width = 0;
    int max_y = 0;
    int min_y = 0;

    const char* character = text;
    while (*character != '\0')
    {
        struct TextCharacter *character_data = textCharacter(*character);
        width += character_data->advance;
        if (character_data->y_offset > max_y) {
            max_y = character_data->y_offset;
        }
        if ((character_data->y_offset - character_data->height) < min_y) {
            min_y = character_data->y_offset - character_data->height;
        }
        character++;
    }

    char *texture_buffer = calloc(width * (max_y - min_y), sizeof(char));
    long advance = 0;

    character = text;
    while (*character != '\0')
    {
        struct TextCharacter *character_data= textCharacter(*character);
        for (int i = 0; i < character_data->height; i++)
        {
            memcpy(texture_buffer + (width * (max_y - character_data->y_offset + i)) + (advance + character_data->x_offset),
                   &character_data->bitmap[i*character_data->width],
                   character_data->width);
        }
        advance += character_data->advance;
        character++;
    }

    *texture = (char*) texture_buffer;
    *texture_width = width;
    *texture_height = max_y - min_y;
}

void textEnd()
{
    FT_Done_Face(face);
    FT_Done_FreeType(library);

    for (int i = 0; i < characters_length; i++)
    {
        free(characters[i].bitmap);
    }
    free(characters);
}
