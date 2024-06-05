import { Typography } from 'antd'

export function Footer() {
  return (
    <div className="flex items-center justify-center">
      <Typography.Text
        className="text-slate-500 text-xs"
        code
      >
        Сделано с ❤️ от{' '}
        <a
          className="!text-blue-400 !font-bold"
          href="https://github.com/QuanticTeam/AtomSkills2024"
        >
          QuanticTeam
        </a>
      </Typography.Text>
    </div>
  )
}
