import { useCallback, useEffect, useState } from "react";

type Toast = { message: string; id: number };

/** A single transient message, announced to screen readers, that hides after a few seconds. */
export function useToast() {
  const [toast, setToast] = useState<Toast | null>(null);
  const show = useCallback(
    (message: string) => setToast({ message, id: Date.now() }),
    [],
  );

  useEffect(() => {
    if (!toast) return;
    const timer = setTimeout(() => setToast(null), 4000);
    return () => clearTimeout(timer);
  }, [toast]);

  const element = (
    <div className="toast-region" role="status" aria-live="polite">
      {toast && (
        <div className="toast" key={toast.id}>
          {toast.message}
        </div>
      )}
    </div>
  );
  return { show, element };
}
