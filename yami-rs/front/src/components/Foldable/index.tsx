import { useState } from "react";
import styles from './index.module.scss';

export default function Foldable({
  children,
  title,
  defaultOpen = false,
}: {
  children: React.ReactNode;
  title: string;
  defaultOpen?: boolean;
}) {
  const [open, setOpen] = useState(defaultOpen);
  return (
    <div className={styles.foldable}>
      <div className={styles.header} onClick={() => setOpen(!open)} data-open={open}>
        <span className={styles.title}>{title}</span>
        <span className={styles.icon}>{open ? '▼' : '▶'}</span>
      </div>
      {open && <div className={styles.body}>{children}</div>}
    </div>
  );
}
