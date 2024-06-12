import { Typography } from 'antd'
import { useTranslation } from 'react-i18next'

export function Footer() {
  const { t } = useTranslation(Footer.name)

  return (
    <div className="flex items-center justify-center">
      <Typography.Text
        className="text-slate-500 text-xs"
        code
      >
        {t('love')}{' '}
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
