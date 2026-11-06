import { expect, test, type Page } from "@playwright/test";

const mod = process.platform === "darwin" ? "Meta" : "Control";

// Lines chosen to stress layout: wrapping, unbroken runs, tabs, trailing and Unicode spaces,
// non-Latin scripts, and an empty line.
const TRICKY = [
  "# Road trip",
  "distance = 420 miles",
  "distance / (65 mph) in hours",
  "",
  "this is a deliberately long line that has to wrap onto more than one visual row of the editor 12 ft",
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  "\tx = 2 μm   ",
  "1 ft + 3　in in cm",
  "π * (2 ft)^2  # area",
  "長さ = 3 m",
  "bogus + 1",
].join("\n");

async function open(page: Page, doc: string) {
  await page.goto("/");
  await page.evaluate((d) => localStorage.setItem("value", d), doc);
  await page.reload();
  await page.evaluate(() => document.fonts.ready);
}

const editor = (page: Page) =>
  page.getByRole("textbox", { name: "calq document" });

async function results(page: Page) {
  return page
    .locator(".result")
    .evaluateAll((els) => els.map((e) => e.textContent ?? ""));
}

/** Replaces the clipboard with a recorder, since headless browsers differ in clipboard access. */
async function recordClipboard(page: Page) {
  await page.addInitScript(() => {
    const w = window as unknown as { copied: string[] };
    w.copied = [];
    Object.defineProperty(navigator, "clipboard", {
      value: {
        writeText: async (t: string) => {
          w.copied.push(t);
        },
      },
    });
  });
}

const copied = (page: Page) =>
  page.evaluate(() => (window as unknown as { copied: string[] }).copied);

// Any page error or console error fails the test. Each worker runs one test at a time.
let errors: string[] = [];

test.beforeEach(async ({ page }) => {
  errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  page.on("console", (m) => {
    if (m.type() === "error") errors.push(m.text());
  });
});

test.afterEach(() => {
  expect(errors).toEqual([]);
});

test.describe("layout", () => {
  test("overlay draws exactly the textarea's text, pixel for pixel", async ({
    page,
  }) => {
    await open(page, TRICKY);
    await editor(page).evaluate((el) => (el as HTMLTextAreaElement).blur());
    await page.addStyleTag({
      content:
        ".editor-input { caret-color: transparent !important; } .result { visibility: hidden; }",
    });
    const box = (await editor(page).boundingBox())!;
    const shot = async (css: string) => {
      const style = await page.addStyleTag({ content: css });
      const png = await page.screenshot({ clip: box, animations: "disabled" });
      await style.evaluate((s) => (s as HTMLElement).remove());
      return png;
    };
    const overlay = await shot(
      ".editor-line, .editor-line * { color: #fff !important; } .editor-input { color: transparent !important; }",
    );
    const textarea = await shot(
      ".editor-line, .editor-line * { color: transparent !important; } .editor-input { color: #fff !important; }",
    );
    const blank = await shot(
      ".editor-line, .editor-line *, .editor-input { color: transparent !important; }",
    );
    expect(overlay.equals(blank)).toBe(false);
    expect(overlay.equals(textarea)).toBe(true);
  });

  test("textarea never scrolls inside itself", async ({ page }) => {
    await open(page, TRICKY);
    await editor(page).press(`${mod}+End`);
    const { scrollTop, scrollHeight, clientHeight } = await editor(
      page,
    ).evaluate((el) => ({
      scrollTop: el.scrollTop,
      scrollHeight: el.scrollHeight,
      clientHeight: el.clientHeight,
    }));
    expect(scrollTop).toBe(0);
    expect(scrollHeight).toBe(clientHeight);
  });

  test("each result sits on its line's first row", async ({ page }) => {
    await open(page, TRICKY);
    const rows = await page.evaluate(() => {
      const lines = [...document.querySelectorAll(".editor-line")];
      const answers = [...document.querySelectorAll(".result")];
      const lh = parseFloat(getComputedStyle(lines[0]).lineHeight);
      return lines.map((l, i) => ({
        line: l.getBoundingClientRect().top,
        result: answers[i].getBoundingClientRect().top,
        resultHeight: answers[i].getBoundingClientRect().height,
        lh,
      }));
    });
    expect(rows).toHaveLength(TRICKY.split("\n").length);
    for (const r of rows) {
      expect(Math.abs(r.result - r.line)).toBeLessThan(0.5);
      expect(r.resultHeight).toBeCloseTo(r.lh, 1);
    }
  });

  test("nothing overflows the viewport horizontally", async ({ page }) => {
    await open(page, TRICKY);
    const overflow = await page.evaluate(
      () => document.documentElement.scrollWidth - window.innerWidth,
    );
    expect(overflow).toBeLessThanOrEqual(0);
  });

  test("errors are muted and never widen the results column", async ({
    page,
  }) => {
    await open(page, "1 ft in m\nbogus + 1");
    const error = page.locator(".result.error");
    await expect(error).toHaveText('Undefined variable "bogus"');
    const before = await page.locator(".result").first().boundingBox();
    await open(
      page,
      "1 ft in m\nbogus + 1 + some + very + long + undefined + names",
    );
    const after = await page.locator(".result").first().boundingBox();
    expect(after!.width).toBe(before!.width);
  });
});

test.describe("editing", () => {
  test("typing on load appends to the document and evaluates live", async ({
    page,
  }) => {
    await open(page, "1 ft in inches");
    await page.keyboard.press("Enter");
    await page.keyboard.type("3 ft in m");
    await expect(editor(page)).toHaveValue("1 ft in inches\n3 ft in m");
    expect(await results(page)).toEqual(["12 inch", "0.91 m"]);
  });

  test("pasting several lines evaluates each", async ({ page }) => {
    await open(page, "");
    await editor(page).focus();
    await page.keyboard.insertText("x = 2 ft\nx * 3\n\nx in inches");
    expect(await results(page)).toEqual(["2 ft", "6 ft", "", "24 inch"]);
  });

  test("a selection across lines can be replaced and undone", async ({
    page,
  }) => {
    await open(page, "1 + 1\n2 + 2\n3 + 3");
    await editor(page).focus();
    await editor(page).evaluate((el) =>
      (el as HTMLTextAreaElement).setSelectionRange(2, 9),
    );
    await page.keyboard.type("*");
    await expect(editor(page)).toHaveValue("1 * 2\n3 + 3");
    expect(await results(page)).toEqual(["2", "6"]);
    await page.keyboard.press(`${mod}+z`);
    await expect(editor(page)).toHaveValue("1 + 1\n2 + 2\n3 + 3");
    expect(await results(page)).toEqual(["2", "4", "6"]);
  });

  test("Clear empties the document and can be undone", async ({ page }) => {
    await open(page, "1 ft in inches\n2 * 2");
    await page.getByRole("button", { name: "Clear" }).click();
    await expect(editor(page)).toHaveValue("");
    await expect(editor(page)).toBeFocused();
    await expect(editor(page)).toHaveAttribute(
      "placeholder",
      "Type a calculation…",
    );
    await page.keyboard.press(`${mod}+z`);
    await expect(editor(page)).toHaveValue("1 ft in inches\n2 * 2");
  });

  test("clicking below the document puts the caret at its end", async ({
    page,
  }) => {
    await open(page, "1 + 1\n2 + 2");
    await editor(page).evaluate((el) => {
      (el as HTMLTextAreaElement).setSelectionRange(0, 0);
      (el as HTMLTextAreaElement).blur();
    });
    const grid = (await page.locator(".editor-grid").boundingBox())!;
    await page.mouse.click(grid.x + 10, grid.y + grid.height + 60);
    await page.keyboard.type("0");
    await expect(editor(page)).toHaveValue("1 + 1\n2 + 20");
  });

  test("the caret stays visible in a long document", async ({ page }) => {
    const doc = Array.from({ length: 80 }, (_, i) => `${i} + 1`).join("\n");
    await open(page, doc);
    const caretVisible = () =>
      page.evaluate(() => {
        const el = document.querySelector(
          ".editor-input",
        ) as HTMLTextAreaElement;
        const row = el.value.slice(0, el.selectionStart).split("\n").length;
        const line = document.querySelectorAll(".editor-line")[row - 1];
        const box = line.getBoundingClientRect();
        const bar = document.querySelector(".app-bar")!.getBoundingClientRect();
        return (
          box.top >= bar.bottom - 1 && box.bottom <= window.innerHeight + 1
        );
      });
    // Loading puts the caret at the end; it must be on screen.
    await expect.poll(caretVisible, { timeout: 1000 }).toBe(true);
    for (let i = 0; i < 79; i++) await page.keyboard.press("ArrowUp");
    await expect.poll(caretVisible, { timeout: 1000 }).toBe(true);
    await page.keyboard.type("x");
    await expect(editor(page)).toHaveValue(/^0 \+ 1x\n1 \+ 1/);
  });

  test("the document survives a reload", async ({ page }) => {
    await open(page, "1 + 1");
    await page.keyboard.type(" + 1");
    await page.reload();
    await expect(editor(page)).toHaveValue("1 + 1 + 1");
  });
});

test.describe("sharing", () => {
  test("Share copies a link that reopens the document", async ({
    page,
    context,
  }) => {
    await recordClipboard(page);
    const doc = "# trip\n420 miles / (65 mph) in hours\n1 + 1 = 2? & #hash";
    await open(page, doc);
    await page.getByRole("button", { name: "Share" }).click();
    await expect(page.getByRole("status")).toHaveText(
      "Copied link to clipboard",
    );
    const [link] = await copied(page);
    const other = await context.newPage();
    await other.goto(link);
    await expect(editor(other)).toHaveValue(doc);
  });

  test("a shared link opens at the top", async ({ page }) => {
    const doc = Array.from({ length: 80 }, (_, i) => `${i} + 1`).join("\n");
    await page.goto(`/#value=${encodeURIComponent(doc)}`);
    expect(await page.evaluate(() => window.scrollY)).toBe(0);
    await page.keyboard.type("x");
    await expect(editor(page)).toHaveValue(/^x0 \+ 1/);
  });

  for (const [format, hash] of [
    [
      "qs, from earlier versions",
      "value=1%20m%2Fs%20%2A%203%20years%0A2%20%2A%202",
    ],
    ["form-encoded", "value=1+m%2Fs+*+3+years%0A2+*+2"],
  ]) {
    test(`links in ${format} format still load`, async ({ page }) => {
      await page.goto("/");
      await page.evaluate(() => localStorage.setItem("value", "saved"));
      await page.goto("about:blank");
      await page.goto(`/#${hash}`);
      await expect(editor(page)).toHaveValue("1 m/s * 3 years\n2 * 2");
      await expect(page.getByRole("status")).toHaveText("Loaded from link");
      expect(new URL(page.url()).hash).toBe("");
      // The saved document is replaced only once the shared one is edited.
      expect(await page.evaluate(() => localStorage.getItem("value"))).toBe(
        "saved",
      );
    });
  }

  test("a link pasted into an open tab loads without a reload", async ({
    page,
  }) => {
    await open(page, "saved");
    await page.evaluate(() => {
      window.location.hash = "value=1%20ft%20in%20inches";
    });
    await expect(editor(page)).toHaveValue("1 ft in inches");
    await expect(page.getByRole("status")).toHaveText("Loaded from link");
    expect(new URL(page.url()).hash).toBe("");
  });

  test("clicking a result copies it", async ({ page }) => {
    await recordClipboard(page);
    await open(page, "1 ft in inches");
    await page.getByRole("button", { name: /12 inch/ }).click();
    expect(await copied(page)).toEqual(["12 inch"]);
    await expect(page.getByRole("status")).toHaveText("Copied 12 inch");
  });

  test("clicking an error shows the whole message", async ({ page }) => {
    await open(page, "bogus + some + very + long + undefined + names");
    await page.locator(".result.error").click();
    await expect(page.getByRole("status")).toHaveText(
      'Undefined variable "bogus"',
    );
  });
});
