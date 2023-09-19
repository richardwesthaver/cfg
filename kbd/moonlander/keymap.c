#include QMK_KEYBOARD_H
#include "version.h"
#include "keymap_french.h"
#include "keymap_spanish.h"
#include "keymap_hungarian.h"
#include "keymap_swedish.h"
// #include "keymap_br_abnt2.h"
#include "keymap_canadian_multilingual.h"
//#include "keymap_german_ch.h"
//#include "keymap_jp.h"
#include "keymap_korean.h"
#include "keymap_bepo.h"
#include "keymap_italian.h"
#include "keymap_slovenian.h"
#include "keymap_lithuanian_azerty.h"
#include "keymap_danish.h"
#include "keymap_norwegian.h"
#include "keymap_portuguese.h"
// #include "keymap_contributions.h"
#include "keymap_czech.h"
#include "keymap_romanian.h"
#include "keymap_russian.h"
#include "keymap_uk.h"
//#include "keymap_estonian.h"
//#include "keymap_belgian.h"
#include "keymap_us_international.h"
//#include "keymap_croatian.h"
//#include "keymap_turkish_q.h"
//#include "keymap_slovak.h"

#define KC_MAC_UNDO LGUI(KC_Z)
#define KC_MAC_CUT LGUI(KC_X)
#define KC_MAC_COPY LGUI(KC_C)
#define KC_MAC_PASTE LGUI(KC_V)
#define KC_PC_UNDO LCTL(KC_Z)
#define KC_PC_CUT LCTL(KC_X)
#define KC_PC_COPY LCTL(KC_C)
#define KC_PC_PASTE LCTL(KC_V)
#define ES_LESS_MAC KC_GRAVE
#define ES_GRTR_MAC LSFT(KC_GRAVE)
#define ES_BSLS_MAC ALGR(KC_6)
#define NO_PIPE_ALT KC_GRAVE
#define NO_BSLS_ALT KC_EQUAL
#define LSA_T(kc) MT(MOD_LSFT | MOD_LALT, kc)
#define BP_NDSH_MAC ALGR(KC_8)
#define SE_SECT_MAC ALGR(KC_6)
#define MOON_LED_LEVEL LED_LEVEL

enum layers {
  MAIN,
};

enum custom_keycodes {
  VRSN = SAFE_RANGE,
  HSV_0_245_245,
  HSV_74_255_206,
  HSV_152_255_255,
};



const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [MAIN] = LAYOUT_moonlander(
    // 1			  
    KC_ESC, KC_EQL, KC_1, KC_2, KC_3, KC_4, KC_5,
    KC_6, KC_7, KC_8, KC_9, KC_0, KC_MINUS, KC_GRAVE,
    // 2
    KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T, TG(1),   
    TG(2), KC_Y, KC_U, KC_I, KC_O, KC_P, KC_BSLS,
    // 3
    KC_LCTL, KC_A, KC_S, KC_D, KC_F, KC_G, KC_LALT,
    KC_ENT, KC_H, KC_J, KC_K, KC_L, KC_SCLN, KC_QUOTE,
    // 4
    KC_LSFT, KC_Z ,KC_X, KC_C, KC_V, KC_B,  
    KC_N, KC_M, KC_COMMA, KC_DOT, KC_SLASH, KC_BSPC,
    // 5
    KC_UP, KC_DOWN, KC_LBRC, KC_LPRN, KC_SPC, C(KC_C),
    A(KC_X), KC_SPC, KC_RPRN, KC_RBRC, KC_LEFT, KC_RIGHT,
    // 6
    CTL_T(KC_COPY), KC_BSPC, KC_LGUI,
    ALT_T(KC_PASTE), RGB_MODE_FORWARD, QK_LOCK
  ),
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (record->event.pressed) {
        switch (keycode) {
        case VRSN:
            SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
            return false;
        }
    }
    return true;
}
