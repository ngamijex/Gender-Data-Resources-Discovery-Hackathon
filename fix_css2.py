p = r'C:\Users\Didier\OneDrive - cenfriglobal\Desktop\GIZ\GDRD\www\styles.css'
with open(p, encoding='utf-8') as f:
    content = f.read()

# 1. Fix dangling `.dh-page` orphan lines (animation without a selector)
content = content.replace(
    ".dh-wizard-body {\n  width          : 100%;\n  padding        : 28px 0 4px;\n}\n  animation      : dhPageIn .22s ease both;\n}\n@keyframes dhPageIn {\n  from { opacity: 0; transform: translateX(6px); }\n  to   { opacity: 1; transform: translateX(0);   }\n}",
    ".dh-wizard-body {\n  width          : 100%;\n  padding        : 28px 0 4px;\n}\n.dh-page {\n  animation      : dhPageIn .22s ease both;\n}\n@keyframes dhPageIn {\n  from { opacity: 0; transform: translateX(6px); }\n  to   { opacity: 1; transform: translateX(0);   }\n}"
)

# 2. Make nav right-aligned (Next always on right, Back on left)
content = content.replace(
    ".dh-nav {\n  display        : flex;\n  align-items    : center;\n  gap            : 8px;\n  padding-top    : 16px;\n  margin-top     : 8px;\n  border-top     : 1px solid var(--color-border-default);\n}",
    ".dh-nav {\n  display        : flex;\n  align-items    : center;\n  justify-content: space-between;\n  gap            : 8px;\n  padding        : 20px 0 28px;\n  margin-top     : 4px;\n  border-top     : 1px solid var(--color-border-default);\n}"
)

# 3. Remove margin-left:auto from next/off buttons (nav is now space-between)
content = content.replace(
    ".dh-nav__btn--next {\n  background     : var(--color-primary-500);\n  color          : #fff;\n  margin-left    : auto;\n  box-shadow     : 0 1px 4px rgba(0,0,0,.12);\n}",
    ".dh-nav__btn--next {\n  background     : var(--color-primary-500);\n  color          : #fff;\n  box-shadow     : 0 1px 4px rgba(0,0,0,.12);\n}"
)
content = content.replace(
    ".dh-nav__btn--off {\n  opacity        : 0.4;\n  cursor         : not-allowed;\n  background     : var(--color-bg-page);\n  border         : 1px solid var(--color-border-default);\n  color          : var(--color-ink-500);\n  margin-left    : auto;\n  box-shadow     : none;\n}",
    ".dh-nav__btn--off {\n  opacity        : 0.4;\n  cursor         : not-allowed;\n  background     : var(--color-bg-page);\n  border         : 1px solid var(--color-border-default);\n  color          : var(--color-ink-500);\n  box-shadow     : none;\n}"
)

# 4. Make sector grid 6 columns (was 2)
old_grid = """.dh-sector-grid  {
  display        : grid;
  grid-template-columns: 1fr 1fr;
  gap            : 8px;"""
new_grid = """.dh-sector-grid  {
  display        : grid;
  grid-template-columns: repeat(6, 1fr);
  gap            : 12px;"""
content = content.replace(old_grid, new_grid)

# 5. Make variables list multi-column
old_vars = """.dh-vars-actions {"""
new_vars = """/* Variables page multi-column layout */
#dh_vars.shiny-input-checkboxgroup .shiny-options-group {
  display               : grid;
  grid-template-columns : repeat(auto-fill, minmax(160px, 1fr));
  gap                   : 2px 16px;
}
.dh-vars-actions {"""
content = content.replace(old_vars, new_vars, 1)

# 6. Remove old .dh-main rule if present
import re
content = re.sub(r'\.dh-main\s*\{[^}]*\}', '', content)

# 7. Add .dh-preview-area before the empty-state section
old_empty = """/* Empty state panel */
.dh-empty {"""
new_empty = """/* ── Full-width preview area (below wizard) ──────────────────────────────── */
.dh-preview-area {
  width          : 100%;
  padding        : 28px 36px 40px;
  display        : flex;
  flex-direction : column;
  gap            : 16px;
  background     : var(--color-bg-page);
}

/* Empty state panel */
.dh-empty {"""
content = content.replace(old_empty, new_empty)

with open(p, 'w', encoding='utf-8') as f:
    f.write(content)
print("CSS updated successfully")
