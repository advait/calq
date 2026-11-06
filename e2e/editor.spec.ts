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
  for (const theme of ["dark", "light"]) {
    test(`overlay draws exactly the textarea's text, pixel for pixel (${theme})`, async ({
      page,
    }) => {
      await page.goto("/");
      await page.evaluate(
        (t) => localStorage.setItem("settings", JSON.stringify({ theme: t })),
        theme,
      );
      await open(page, TRICKY);
      await editor(page).evaluate((el) => (el as HTMLTextAreaElement).blur());
      await page.addStyleTag({
        content:
          ".editor-input { caret-color: transparent !important; } .result { visibility: hidden; }",
      });
      const box = (await editor(page).boundingBox())!;
      const shot = async (css: string) => {
        const style = await page.addStyleTag({ content: css });
        const png = await page.screenshot({
          clip: box,
          animations: "disabled",
        });
        await style.evaluate((s) => (s as HTMLElement).remove());
        return png;
      };
      const overlay = await shot(
        ".editor-line, .editor-line * { color: #f0f !important; } .editor-input { color: transparent !important; }",
      );
      const textarea = await shot(
        ".editor-line, .editor-line * { color: transparent !important; } .editor-input { color: #f0f !important; }",
      );
      const blank = await shot(
        ".editor-line, .editor-line *, .editor-input { color: transparent !important; }",
      );
      expect(overlay.equals(blank)).toBe(false);
      expect(overlay.equals(textarea)).toBe(true);
    });
  }

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

test.describe("first visit", () => {
  test("a new visitor sees the example, and every line of it works", async ({
    page,
  }) => {
    await page.goto("/");
    await expect(editor(page)).toHaveValue(/^# Welcome to calq/);
    await expect(page.locator(".result.error")).toHaveCount(0);
    const answers = await page
      .locator(".result.success")
      .evaluateAll((els) => els.map((e) => e.textContent));
    expect(answers).toContain("709.76 ml");
    expect(answers.length).toBeGreaterThanOrEqual(12);
  });

  test("a saved document wins over the example", async ({ page }) => {
    await open(page, "1 + 1");
    await page.reload();
    await expect(editor(page)).toHaveValue("1 + 1");
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

  test("a long wrapped last line still opens with the caret on screen", async ({
    page,
  }) => {
    await open(page, "# " + "abc ".repeat(750));
    const bottom = await page.evaluate(() => {
      const lines = document.querySelectorAll(".editor-line");
      return lines[lines.length - 1].getBoundingClientRect().bottom;
    });
    const viewport = page.viewportSize()!.height;
    expect(bottom).toBeGreaterThan(0);
    expect(bottom).toBeLessThanOrEqual(viewport);
  });

  test("line breaks other than \\n are normalized", async ({ page }) => {
    await open(page, "1\r2\r\n3 + 3\u2028x\vy");
    await expect(editor(page)).toHaveValue("1\n2\n3 + 3\nx\ny");
    await expect(page.locator(".editor-line")).toHaveCount(5);
    const { scrollHeight, clientHeight } = await editor(page).evaluate(
      (el) => ({
        scrollHeight: el.scrollHeight,
        clientHeight: el.clientHeight,
      }),
    );
    expect(scrollHeight).toBe(clientHeight);
  });

  test("an input that once crashed the engine shows an error", async ({
    page,
  }) => {
    await page.goto("/#value=1%C2%A0e3");
    await expect(editor(page)).toHaveValue("1\u00a0e3");
    await expect(page.locator(".result.error")).toHaveText(
      'Undefined variable "e3"',
    );
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

  test("a link pasted into an open tab is read from the top", async ({
    page,
  }) => {
    const long = (tag: string) =>
      Array.from({ length: 80 }, (_, i) => `${tag}${i} = ${i}`).join("\n");
    await open(page, long("a"));
    expect(await page.evaluate(() => window.scrollY)).toBeGreaterThan(0);
    await page.evaluate(
      (doc) => (window.location.hash = `value=${encodeURIComponent(doc)}`),
      long("b"),
    );
    await expect(editor(page)).toHaveValue(long("b"));
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBe(0);
    await page.keyboard.type("x");
    await expect(editor(page)).toHaveValue(/^xb0 = 0/);
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

test.describe("settings", () => {
  const settingsButton = (page: Page) =>
    page.getByRole("button", { name: "Settings" });
  const panel = (page: Page) => page.getByRole("dialog", { name: "Settings" });

  test("the popover opens below the app bar and closes on Escape", async ({
    page,
  }) => {
    await open(page, "1 + 1");
    await expect(panel(page)).toBeHidden();
    await settingsButton(page).click();
    await expect(panel(page)).toBeVisible();
    const box = (await panel(page).boundingBox())!;
    const bar = (await page.locator(".app-bar").boundingBox())!;
    const viewport = page.viewportSize()!;
    expect(box.y).toBeGreaterThanOrEqual(bar.y + bar.height);
    expect(box.x).toBeGreaterThanOrEqual(0);
    expect(box.x + box.width).toBeLessThanOrEqual(viewport.width);
    await page.keyboard.press("Escape");
    await expect(panel(page)).toBeHidden();
  });

  test("clicking outside closes the popover", async ({ page }) => {
    await open(page, "1 + 1");
    await settingsButton(page).click();
    await expect(panel(page)).toBeVisible();
    const box = (await page.locator(".editor-grid").boundingBox())!;
    await page.mouse.click(box.x + 5, box.y + 5);
    await expect(panel(page)).toBeHidden();
  });

  test("decimal places round every answer and are remembered", async ({
    page,
  }) => {
    await open(page, "1/7\n420 miles / (65 mph) in hours\n1 ft in m");
    expect(await results(page)).toEqual(["0.14", "6.46 hr", "0.3 m"]);
    await settingsButton(page).click();
    await panel(page).getByLabel("Decimal places").selectOption("4");
    await expect(panel(page)).toContainText("1/7 = 0.1429");
    expect(await results(page)).toEqual(["0.1429", "6.4615 hr", "0.3048 m"]);
    await panel(page).getByLabel("Decimal places").selectOption("0");
    expect(await results(page)).toEqual(["0", "6 hr", "0 m"]);
    await panel(page).getByLabel("Decimal places").selectOption("all");
    expect((await results(page))[0]).toBe("0.14285714285714285714");
    await page.reload();
    expect((await results(page))[0]).toBe("0.14285714285714285714");
  });

  test("decimal places don't change what Share copies", async ({ page }) => {
    await recordClipboard(page);
    await open(page, "1/7");
    await settingsButton(page).click();
    await panel(page).getByLabel("Decimal places").selectOption("6");
    await page.keyboard.press("Escape");
    await page.getByRole("button", { name: "Share" }).click();
    const [link] = await copied(page);
    expect(link).toMatch(/#value=1%2F7$/);
  });

  test("the light theme applies at once and survives a reload", async ({
    page,
  }) => {
    await open(page, "1 + 1");
    const background = () =>
      page.evaluate(() => getComputedStyle(document.body).backgroundColor);
    expect(await background()).toBe("rgb(29, 31, 33)");
    await settingsButton(page).click();
    await panel(page).getByText("Light", { exact: true }).click();
    expect(await background()).toBe("rgb(255, 255, 255)");
    await expect(page.locator('meta[name="theme-color"]')).toHaveAttribute(
      "content",
      "#f6f6f6",
    );
    // Hold back the app's script: the theme must already be set before it runs.
    await page.route("**/assets/*.js", () => {});
    await page.reload({ waitUntil: "commit" });
    await page.waitForFunction(() => document.body !== null);
    expect(
      await page.evaluate(
        () => document.getElementById("root")!.childElementCount,
      ),
    ).toBe(0);
    expect(
      await page.evaluate(() => document.documentElement.dataset.theme),
    ).toBe("light");
    expect(await background()).toBe("rgb(255, 255, 255)");
    await expect(page.locator('meta[name="theme-color"]')).toHaveAttribute(
      "content",
      "#f6f6f6",
    );
  });

  test("the system theme follows the OS", async ({ page }) => {
    await page.emulateMedia({ colorScheme: "light" });
    await open(page, "1 + 1");
    await settingsButton(page).click();
    await panel(page).getByText("System", { exact: true }).click();
    const theme = () =>
      page.evaluate(() => document.documentElement.dataset.theme);
    expect(await theme()).toBe("light");
    await page.emulateMedia({ colorScheme: "dark" });
    await expect.poll(theme).toBe("dark");
  });

  test("settings are reachable by keyboard", async ({ page }) => {
    await open(page, "1/7");
    await settingsButton(page).focus();
    await page.keyboard.press("Enter");
    await expect(panel(page)).toBeVisible();
    await page.keyboard.press("Tab");
    await expect(panel(page).getByLabel("Decimal places")).toBeFocused();
    await page.keyboard.press("Escape");
    await expect(panel(page)).toBeHidden();
    await expect(settingsButton(page)).toBeFocused();
  });

  test("All digits keeps every significant digit", async ({ page }) => {
    await open(page, "1e-21\n1/7e10");
    await page.getByRole("button", { name: "Settings" }).click();
    await panel(page).getByLabel("Decimal places").selectOption("all");
    expect(await results(page)).toEqual(["1e-21", "1.4285714285714285714e-11"]);
  });

  test("the popover fits a 320px screen", async ({ page }) => {
    await page.setViewportSize({ width: 320, height: 568 });
    await open(page, "1 + 1");
    await settingsButton(page).click();
    const box = (await panel(page).boundingBox())!;
    expect(box.x).toBeGreaterThanOrEqual(0);
    expect(box.x + box.width).toBeLessThanOrEqual(320);
  });
});

test.describe("without storage", () => {
  for (const [what, script] of [
    [
      "blocked",
      () =>
        Object.defineProperty(window, "localStorage", {
          get() {
            throw new DOMException("blocked", "SecurityError");
          },
        }),
    ],
    [
      "full",
      () => {
        Storage.prototype.setItem = () => {
          throw new DOMException("full", "QuotaExceededError");
        };
      },
    ],
  ] as const) {
    test(`calq still works when storage is ${what}`, async ({ page }) => {
      await page.addInitScript(script);
      await page.goto("/");
      await editor(page).focus();
      await page.keyboard.press(`${mod}+End`);
      await page.keyboard.type("\n1/7");
      expect((await results(page)).at(-1)).toBe("0.14");
      await page.getByRole("button", { name: "Settings" }).click();
      const panel = page.getByRole("dialog", { name: "Settings" });
      await panel.getByLabel("Decimal places").selectOption("4");
      expect((await results(page)).at(-1)).toBe("0.1429");
      await panel.getByText("Light", { exact: true }).click();
      expect(
        await page.evaluate(() => document.documentElement.dataset.theme),
      ).toBe("light");
    });
  }
});
