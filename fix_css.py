p = r'C:\Users\Didier\OneDrive - cenfriglobal\Desktop\GIZ\GDRD\www\styles.css'
with open(p, encoding='utf-8') as f:
    lines = f.readlines()

new_block = """\
/* -- Body layout: stacked (wizard top, preview bottom) ------------------- */
.dh-body {
  display        : flex;
  flex-direction : column;
  width          : 100%;
}

/* -- Full-width wizard panel ------------------------------------------------ */
.dh-wizard {
  width          : 100%;
  background     : var(--color-bg-surface);
  border-bottom  : 2px solid var(--color-border-default);
  padding        : 28px 36px 0;
  display        : flex;
  flex-direction : column;
}

/* -- Wizard progress track -------------------------------------------------- */
.dh-progress {
  display        : flex;
  align-items    : center;
  padding-bottom : 24px;
  border-bottom  : 1px solid var(--color-border-default);
}
.dh-prog__step {
  display        : flex;
  flex-direction : column;
  align-items    : center;
  gap            : 6px;
  flex           : 1;
}
.dh-prog__num {
  width          : 36px;
  height         : 36px;
  border-radius  : 50%;
  background     : var(--color-bg-page);
  border         : 2px solid var(--color-border-default);
  color          : var(--color-ink-400);
  font-size      : 14px;
  font-weight    : 800;
  display        : flex;
  align-items    : center;
  justify-content: center;
  transition     : background .2s, border-color .2s, color .2s;
}
.dh-prog__label {
  font-size      : 11px;
  font-weight    : 700;
  color          : var(--color-ink-400);
  text-transform : uppercase;
  letter-spacing : .06em;
  white-space    : nowrap;
  transition     : color .2s;
}
.dh-prog__step--active .dh-prog__num   { background: var(--color-primary-500); border-color: var(--color-primary-500); color: #fff; }
.dh-prog__step--active .dh-prog__label { color: var(--color-primary-500); font-weight: 800; }
.dh-prog__step--done   .dh-prog__num   { background: var(--color-ink-900); border-color: var(--color-ink-900); color: #fff; }
.dh-prog__step--done   .dh-prog__label { color: var(--color-ink-600); }
.dh-prog__connector {
  flex           : 1;
  height         : 2px;
  background     : var(--color-border-default);
  margin-bottom  : 16px;
  min-width      : 32px;
  transition     : background .25s;
}
.dh-prog__connector--done { background: var(--color-ink-900); }

/* -- Wizard body: full-width page content ----------------------------------- */
.dh-wizard-body {
  width          : 100%;
  padding        : 28px 0 4px;
}
"""

# Replace lines 4355..4444 (0-indexed, inclusive)
start = 4355
end   = 4445
lines[start:end] = [new_block]

with open(p, 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("Done. New line count:", len(lines))
