(module doodle-colors
        (aliceblue
         antiquewhite
         aqua
         aquamarine
         azure
         beige
         bisque
         black
         blanchedalmond
         blue
         blueviolet
         brown
         burlywood
         cadetblue
         chartreuse
         chocolate
         coral
         cornflowerblue
         cornsilk
         crimson
         cyan
         darkblue
         darkcyan
         darkgoldenrod
         darkgray
         darkgreen
         darkgrey
         darkkhaki
         darkmagenta
         darkolivegreen
         darkorange
         darkorchid
         darkred
         darksalmon
         darkseagreen
         darkslateblue
         darkslategray
         darkslategrey
         darkturquoise
         darkviolet
         deeppink
         deepskyblue
         dimgray
         dimgrey
         dodgerblue
         firebrick
         floralwhite
         forestgreen
         fuchsia
         gainsboro
         ghostwhite
         gold
         goldenrod
         gray
         grey
         green
         greenyellow
         honeydew
         hotpink
         indianred
         indigo
         ivory
         khaki
         lavender
         lavenderblush
         lawngreen
         lemonchiffon
         lightblue
         lightcoral
         lightcyan
         lightgoldenrodyellow
         lightgray
         lightgreen
         lightgrey
         lightpink
         lightsalmon
         lightseagreen
         lightskyblue
         lightslategray
         lightslategrey
         lightsteelblue
         lightyellow
         lime
         limegreen
         linen
         magenta
         maroon
         mediumaquamarine
         mediumblue
         mediumorchid
         mediumpurple
         mediumseagreen
         mediumslateblue
         mediumspringgreen
         mediumturquoise
         mediumvioletred
         midnightblue
         mistyrose
         moccasin
         navajowhite
         navy
         oldlace
         olive
         olivedrab
         orange
         orangered
         orchid
         palegoldenrod
         palegreen
         paleturquoise
         palevioletred
         papayawhip
         peachpuff
         peru
         pink
         plum
         powderblue
         purple
         red
         rosybrown
         royalblue
         saddlebrown
         salmon
         sandybrown
         seagreen
         seashell
         sienna
         silver
         skyblue
         slateblue
         slategray
         slategrey
         snow
         springgreen
         steelblue
         teal
         thistle
         tomato
         turquoise
         violet
         wheat
         white
         whitesmoke
         yellow
         yellowgreen)

        (import chicken scheme)

        (define aliceblue '(0.941176470588235 0.972549019607843 1 1))
        (define antiquewhite '(0.980392156862745 0.92156862745098 0.843137254901961 1))
        (define aqua '(0 1 1 1))
        (define aquamarine '(0.498039215686275 1 0.831372549019608 1))
        (define azure '(0.941176470588235 1 1 1))
        (define beige '(0.96078431372549 0.96078431372549 0.862745098039216 1))
        (define bisque '(1 0.894117647058824 0.768627450980392 1))
        (define black '(0 0 0 1))
        (define blanchedalmond '(1 0.92156862745098 0.803921568627451 1))
        (define blue '(0 0 1 1))
        (define blueviolet '(0.541176470588235 0.168627450980392 0.886274509803922 1))
        (define brown '(0.647058823529412 0.164705882352941 0.164705882352941 1))
        (define burlywood '(0.870588235294118 0.72156862745098 0.529411764705882 1))
        (define cadetblue '(0.372549019607843 0.619607843137255 0.627450980392157 1))
        (define chartreuse '(0.498039215686275 1 0 1))
        (define chocolate '(0.823529411764706 0.411764705882353 0.117647058823529 1))
        (define coral '(1 0.498039215686275 0.313725490196078 1))
        (define cornflowerblue '(0.392156862745098 0.584313725490196 0.929411764705882 1))
        (define cornsilk '(1 0.972549019607843 0.862745098039216 1))
        (define crimson '(0.862745098039216 0.0784313725490196 0.235294117647059 1))
        (define cyan '(0 1 1 1))
        (define darkblue '(0 0 0.545098039215686 1))
        (define darkcyan '(0 0.545098039215686 0.545098039215686 1))
        (define darkgoldenrod '(0.72156862745098 0.525490196078431 0.0431372549019608 1))
        (define darkgray '(0.662745098039216 0.662745098039216 0.662745098039216 1))
        (define darkgreen '(0 0.392156862745098 0 1))
        (define darkgrey '(0.662745098039216 0.662745098039216 0.662745098039216 1))
        (define darkkhaki '(0.741176470588235 0.717647058823529 0.419607843137255 1))
        (define darkmagenta '(0.545098039215686 0 0.545098039215686 1))
        (define darkolivegreen '(0.333333333333333 0.419607843137255 0.184313725490196 1))
        (define darkorange '(1 0.549019607843137 0 1))
        (define darkorchid '(0.6 0.196078431372549 0.8 1))
        (define darkred '(0.545098039215686 0 0 1))
        (define darksalmon '(0.913725490196078 0.588235294117647 0.47843137254902 1))
        (define darkseagreen '(0.56078431372549 0.737254901960784 0.56078431372549 1))
        (define darkslateblue '(0.282352941176471 0.23921568627451 0.545098039215686 1))
        (define darkslategray '(0.184313725490196 0.309803921568627 0.309803921568627 1))
        (define darkslategrey '(0.184313725490196 0.309803921568627 0.309803921568627 1))
        (define darkturquoise '(0 0.807843137254902 0.819607843137255 1))
        (define darkviolet '(0.580392156862745 0 0.827450980392157 1))
        (define deeppink '(1 0.0784313725490196 0.576470588235294 1))
        (define deepskyblue '(0 0.749019607843137 1 1))
        (define dimgray '(0.411764705882353 0.411764705882353 0.411764705882353 1))
        (define dimgrey '(0.411764705882353 0.411764705882353 0.411764705882353 1))
        (define dodgerblue '(0.117647058823529 0.564705882352941 1 1))
        (define firebrick '(0.698039215686274 0.133333333333333 0.133333333333333 1))
        (define floralwhite '(1 0.980392156862745 0.941176470588235 1))
        (define forestgreen '(0.133333333333333 0.545098039215686 0.133333333333333 1))
        (define fuchsia '(1 0 1 1))
        (define gainsboro '(0.862745098039216 0.862745098039216 0.862745098039216 1))
        (define ghostwhite '(0.972549019607843 0.972549019607843 1 1))
        (define gold '(1 0.843137254901961 0 1))
        (define goldenrod '(0.854901960784314 0.647058823529412 0.125490196078431 1))
        (define gray '(0.501960784313725 0.501960784313725 0.501960784313725 1))
        (define grey '(0.501960784313725 0.501960784313725 0.501960784313725 1))
        (define green '(0 0.501960784313725 0 1))
        (define greenyellow '(0.67843137254902 1 0.184313725490196 1))
        (define honeydew '(0.941176470588235 1 0.941176470588235 1))
        (define hotpink '(1 0.411764705882353 0.705882352941177 1))
        (define indianred '(0.803921568627451 0.36078431372549 0.36078431372549 1))
        (define indigo '(0.294117647058824 0 0.509803921568627 1))
        (define ivory '(1 1 0.941176470588235 1))
        (define khaki '(0.941176470588235 0.901960784313726 0.549019607843137 1))
        (define lavender '(0.901960784313726 0.901960784313726 0.980392156862745 1))
        (define lavenderblush '(1 0.941176470588235 0.96078431372549 1))
        (define lawngreen '(0.486274509803922 0.988235294117647 0 1))
        (define lemonchiffon '(1 0.980392156862745 0.803921568627451 1))
        (define lightblue '(0.67843137254902 0.847058823529412 0.901960784313726 1))
        (define lightcoral '(0.941176470588235 0.501960784313725 0.501960784313725 1))
        (define lightcyan '(0.87843137254902 1 1 1))
        (define lightgoldenrodyellow '(0.980392156862745 0.980392156862745 0.823529411764706 1))
        (define lightgray '(0.827450980392157 0.827450980392157 0.827450980392157 1))
        (define lightgreen '(0.564705882352941 0.933333333333333 0.564705882352941 1))
        (define lightgrey '(0.827450980392157 0.827450980392157 0.827450980392157 1))
        (define lightpink '(1 0.713725490196078 0.756862745098039 1))
        (define lightsalmon '(1 0.627450980392157 0.47843137254902 1))
        (define lightseagreen '(0.125490196078431 0.698039215686274 0.666666666666667 1))
        (define lightskyblue '(0.529411764705882 0.807843137254902 0.980392156862745 1))
        (define lightslategray '(0.466666666666667 0.533333333333333 0.6 1))
        (define lightslategrey '(0.466666666666667 0.533333333333333 0.6 1))
        (define lightsteelblue '(0.690196078431373 0.768627450980392 0.870588235294118 1))
        (define lightyellow '(1 1 0.87843137254902 1))
        (define lime '(0 1 0 1))
        (define limegreen '(0.196078431372549 0.803921568627451 0.196078431372549 1))
        (define linen '(0.980392156862745 0.941176470588235 0.901960784313726 1))
        (define magenta '(1 0 1 1))
        (define maroon '(0.501960784313725 0 0 1))
        (define mediumaquamarine '(0.4 0.803921568627451 0.666666666666667 1))
        (define mediumblue '(0 0 0.803921568627451 1))
        (define mediumorchid '(0.729411764705882 0.333333333333333 0.827450980392157 1))
        (define mediumpurple '(0.576470588235294 0.43921568627451 0.858823529411765 1))
        (define mediumseagreen '(0.235294117647059 0.701960784313725 0.443137254901961 1))
        (define mediumslateblue '(0.482352941176471 0.407843137254902 0.933333333333333 1))
        (define mediumspringgreen '(0 0.980392156862745 0.603921568627451 1))
        (define mediumturquoise '(0.282352941176471 0.819607843137255 0.8 1))
        (define mediumvioletred '(0.780392156862745 0.0823529411764706 0.52156862745098 1))
        (define midnightblue '(0.0980392156862745 0.0980392156862745 0.43921568627451 1))
        (define mistyrose '(1 0.894117647058824 0.882352941176471 1))
        (define moccasin '(1 0.894117647058824 0.709803921568627 1))
        (define navajowhite '(1 0.870588235294118 0.67843137254902 1))
        (define navy '(0 0 0.501960784313725 1))
        (define oldlace '(0.992156862745098 0.96078431372549 0.901960784313726 1))
        (define olive '(0.501960784313725 0.501960784313725 0 1))
        (define olivedrab '(0.419607843137255 0.556862745098039 0.137254901960784 1))
        (define orange '(1 0.647058823529412 0 1))
        (define orangered '(1 0.270588235294118 0 1))
        (define orchid '(0.854901960784314 0.43921568627451 0.83921568627451 1))
        (define palegoldenrod '(0.933333333333333 0.909803921568627 0.666666666666667 1))
        (define palegreen '(0.596078431372549 0.984313725490196 0.596078431372549 1))
        (define paleturquoise '(0.686274509803922 0.933333333333333 0.933333333333333 1))
        (define palevioletred '(0.858823529411765 0.43921568627451 0.576470588235294 1))
        (define papayawhip '(1 0.937254901960784 0.835294117647059 1))
        (define peachpuff '(1 0.854901960784314 0.725490196078431 1))
        (define peru '(0.803921568627451 0.52156862745098 0.247058823529412 1))
        (define pink '(1 0.752941176470588 0.796078431372549 1))
        (define plum '(0.866666666666667 0.627450980392157 0.866666666666667 1))
        (define powderblue '(0.690196078431373 0.87843137254902 0.901960784313726 1))
        (define purple '(0.501960784313725 0 0.501960784313725 1))
        (define red '(1 0 0 1))
        (define rosybrown '(0.737254901960784 0.56078431372549 0.56078431372549 1))
        (define royalblue '(0.254901960784314 0.411764705882353 0.882352941176471 1))
        (define saddlebrown '(0.545098039215686 0.270588235294118 0.0745098039215686 1))
        (define salmon '(0.980392156862745 0.501960784313725 0.447058823529412 1))
        (define sandybrown '(0.956862745098039 0.643137254901961 0.376470588235294 1))
        (define seagreen '(0.180392156862745 0.545098039215686 0.341176470588235 1))
        (define seashell '(1 0.96078431372549 0.933333333333333 1))
        (define sienna '(0.627450980392157 0.32156862745098 0.176470588235294 1))
        (define silver '(0.752941176470588 0.752941176470588 0.752941176470588 1))
        (define skyblue '(0.529411764705882 0.807843137254902 0.92156862745098 1))
        (define slateblue '(0.415686274509804 0.352941176470588 0.803921568627451 1))
        (define slategray '(0.43921568627451 0.501960784313725 0.564705882352941 1))
        (define slategrey '(0.43921568627451 0.501960784313725 0.564705882352941 1))
        (define snow '(1 0.980392156862745 0.980392156862745 1))
        (define springgreen '(0 1 0.498039215686275 1))
        (define steelblue '(0.274509803921569 0.509803921568627 0.705882352941177 1))
        (define teal '(0 0.501960784313725 0.501960784313725 1))
        (define thistle '(0.847058823529412 0.749019607843137 0.847058823529412 1))
        (define tomato '(1 0.388235294117647 0.27843137254902 1))
        (define turquoise '(0.250980392156863 0.87843137254902 0.815686274509804 1))
        (define violet '(0.933333333333333 0.509803921568627 0.933333333333333 1))
        (define wheat '(0.96078431372549 0.870588235294118 0.701960784313725 1))
        (define white '(1 1 1 1))
        (define whitesmoke '(0.96078431372549 0.96078431372549 0.96078431372549 1))
        (define yellow '(1 1 0 1))
        (define yellowgreen '(0.603921568627451 0.803921568627451 0.196078431372549 1)))
