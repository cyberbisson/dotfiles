
/* My own custom color set:
 */

define_color ("black",                  0,   0,     0);
define_color ("blue",                   0,   0,   255);
define_color ("cadet blue",             95,  158, 160);
define_color ("dark goldenrod",         184, 134,  11);
define_color ("dark green",             0,   100,   0);
define_color ("dark olive green",       85,  107,  47);
define_color ("firebrick",              178, 34,   34);
define_color ("forest green",           34,  139,  34);
define_color ("OrangeRed2",             238, 64,    0);
define_color ("orchid",                 218, 112, 214);
define_color ("purple",                 160, 32,  240);
define_color ("rosy brown",             188, 143, 143);
define_color ("steel blue",             70,  30,  180);
define_color ("cadet blue",             95,  158, 160);
define_color ("chocolate",              210, 105,  30);
define_color ("DodgerBlue4",            16,  78,  139);
define_color ("maroon",                 176, 48,   96);

define_color ("DarkSeaGreen",           139, 179, 129);
define_color ("Goldenrod",              237, 218, 116);
define_color ("Aquamarine",             67,  183, 186);
define_color ("SeaGreen2",              100, 233, 134);
define_color ("Coral",                  247, 101,  65);
define_color ("DarkSlateGray1",         154, 254, 255);

state style_mbisson
{
    BEGIN
    {
        call(default_faces);
        verbose_highlighting = true;

        if (color)
        {
            /* Set colors. */
            face_bold[fg_color]          = language_color("black");
            face_italic[fg_color]        = language_color("black");
            face_bold_italic[fg_color]   = language_color("black");
            face_comment[fg_color]       = language_color("dark green");
            face_function_name[fg_color] = language_color("OrangeRed2");
            face_variable_name[fg_color] = language_color("steel blue");
            face_keyword[fg_color]       = language_color("firebrick");
            face_reference[fg_color]     = language_color("cadet blue");
            face_string[fg_color]        = language_color("chocolate");
            face_builtin[fg_color]       = language_color("DodgerBlue4");
            face_type[fg_color]          = language_color("maroon");
            face_highlight[bg_color]     = language_color("DarkSeaGreen2");
        }

        /* And that's it. */
        return;
    }
}


/*
Local variables:
mode: c
End:
*/
